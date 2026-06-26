# ── StyleSync — Shiny server ─────────────────────────────────────────────
library(shiny)

source("launch_dashboard.R")
load_fashion_modules()

server <- function(input, output, session) {

  output$top_preview <- renderImage({
    req(input$top_img)
    list(
      src = input$top_img$datapath,
      contentType = input$top_img$type,
      alt = "Top garment preview"
    )
  }, deleteFile = FALSE)

  output$bottom_preview <- renderImage({
    req(input$bottom_img)
    list(
      src = input$bottom_img$datapath,
      contentType = input$bottom_img$type,
      alt = "Bottom garment preview"
    )
  }, deleteFile = FALSE)

  output$face_preview <- renderImage({
    req(input$face_img)
    list(
      src = input$face_img$datapath,
      contentType = input$face_img$type,
      alt = "Face photo preview"
    )
  }, deleteFile = FALSE)

  # ── Run analysis on button click ───────────────────────────────────────────
  result <- eventReactive(input$run_btn, {

    req(input$top_img, input$bottom_img)

    face_path  <- if (!is.null(input$face_img)) input$face_img$datapath else NULL
    body_shape <- if (input$body_shape == "not provided") NULL else input$body_shape

    withProgress(message = "Analysing outfit...", value = 0, {
      setProgress(0.2, detail = "Extracting colours")
      setProgress(0.5, detail = "Running compatibility checks")

      tryCatch(
        predict_outfit(
          top_img    = input$top_img$datapath,
          bottom_img = input$bottom_img$datapath,
          face_img   = face_path,
          mood       = input$mood,
          undertone  = input$undertone,
          body_shape = body_shape
        ),
        error = function(e) list(error = e$message)
      )
    })
  })

  # ── Render results UI ──────────────────────────────────────────────────────
  output$results_ui <- renderUI({

    if (is.null(result())) {
      return(div(style = "color:#999; font-size:14px; padding:40px 0; text-align:center;",
                 "Upload images and click Analyse outfit to get started."))
    }

    res <- result()

    # Show error if pipeline failed
    if (!is.null(res$error)) {
      return(div(class = "error-box",
                 tags$b("Something went wrong: "), res$error))
    }

    # ── Score colour ─────────────────────────────────────────────────────────
    score      <- res$Compatibility_Score
    score_col  <- if (score >= 75) "#3B6D11" else if (score >= 50) "#854F0B" else "#A32D2D"
    score_bg   <- if (score >= 75) "#EAF3DE"  else if (score >= 50) "#FAEEDA"  else "#FCEBEB"
    score_tag  <- if (score >= 75) "Great match!" else if (score >= 50) "Decent match" else "Consider changing"

    conf_pct   <- paste0(round(res$Overall_Confidence * 100), "%")

    # ── Tag helper ───────────────────────────────────────────────────────────
    tag_cls <- function(val) {
      if (is.null(val) || val == "unknown" || val == "not provided") "tag tag-amber"
      else "tag tag-green"
    }
    fmt <- function(val) if (is.null(val) || val == "") "not provided" else val

    tagList(

      # Score card
      div(class = "score-box",
          style = paste0("background:", score_bg, ";"),
          div(class = "score-num", style = paste0("color:", score_col, ";"), score),
          div(class = "score-lbl", "Compatibility score out of 100"),
          div(style = "margin-top:8px;",
              span(class = paste0("tag ", if(score>=75)"tag-green" else if(score>=50)"tag-amber" else "tag-red"),
                   score_tag)),
          div(style = "margin-top:12px; font-size:12px; color:#666;", "Analysis confidence"),
          div(class = "conf-bar-bg",
              div(class = "conf-bar",
                  style = paste0("width:", conf_pct, "; background:", score_col, ";"))),
          div(style = "font-size:12px; color:#666;", conf_pct)
      ),

      # Colour details
      div(class = "panel",
          div(class = "panel-title", "Colour analysis"),
          div(class = "detail-row",
              span(class="detail-key","Top colour type"),
              span(class="detail-val", span(class=tag_cls(res$Top_Color_Type), fmt(res$Top_Color_Type)))),
          div(class = "detail-row",
              span(class="detail-key","Bottom colour type"),
              span(class="detail-val", span(class=tag_cls(res$Bottom_Color_Type), fmt(res$Bottom_Color_Type)))),
          div(class = "detail-row",
              span(class="detail-key","Palette style"),
              span(class="detail-val", span(class="tag tag-blue", fmt(res$Palette_Style)))),
          div(class = "detail-row",
              span(class="detail-key","PCA colour clusters"),
              span(class="detail-val", res$PCA_Optimal_K))
      ),

      # Silhouette + face details
      div(class = "panel",
          div(class = "panel-title", "Fit & face"),
          div(class = "detail-row",
              span(class="detail-key","Top silhouette"),
              span(class="detail-val", span(class=tag_cls(res$Top_Shape), fmt(res$Top_Shape)))),
          div(class = "detail-row",
              span(class="detail-key","Bottom silhouette"),
              span(class="detail-val", span(class=tag_cls(res$Bottom_Shape), fmt(res$Bottom_Shape)))),
          div(class = "detail-row",
              span(class="detail-key","Neckline"),
              span(class="detail-val", span(class=tag_cls(res$Neckline), fmt(res$Neckline)))),
          div(class = "detail-row",
              span(class="detail-key","Face shape"),
              span(class="detail-val", span(class=tag_cls(res$Face_Shape), fmt(res$Face_Shape)))),
          div(class = "detail-row",
              span(class="detail-key","Body shape"),
              span(class="detail-val", fmt(res$Body_Shape)))
      ),

      # Context
      div(class = "panel",
          div(class = "panel-title", "Context"),
          div(class = "detail-row",
              span(class="detail-key","Mood"),
              span(class="detail-val", fmt(res$Mood))),
          div(class = "detail-row",
              span(class="detail-key","Undertone"),
              span(class="detail-val", fmt(res$Undertone))),
          if (!is.null(res$Skin_Confidence) && !is.na(res$Skin_Confidence))
            div(class = "detail-row",
                span(class="detail-key","Skin detection confidence"),
                span(class="detail-val", paste0(round(res$Skin_Confidence * 100), "%")))
      )
    )
  })
}
