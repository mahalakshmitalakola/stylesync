# Stylesync - Shiny UI
library(shiny)

preview_slot <- function(title, output_id, optional = FALSE) {
  div(class = "preview-item",
      div(class = "preview-head",
          span(title),
          if (optional) span(class = "preview-note", "optional")),
      div(class = "preview-frame",
          imageOutput(output_id, height = "100%")))
}

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      body { font-family: sans-serif; background: #f8f8f6; color: #1a1a1a; }
      .main-title { font-size: 22px; font-weight: 500; margin-bottom: 4px; }
      .sub-title   { font-size: 14px; color: #666; margin-bottom: 24px; }
      .panel       { background: #fff; border: 1px solid #e5e5e0; border-radius: 8px;
                     padding: 20px; margin-bottom: 16px; }
      .panel-title { font-size: 14px; font-weight: 500; margin-bottom: 14px; color: #444; }
      .score-box   { background: #f0f7ea; border-radius: 8px; padding: 20px;
                     text-align: center; margin-bottom: 16px; }
      .score-num   { font-size: 56px; font-weight: 500; color: #3B6D11; line-height: 1; }
      .score-lbl   { font-size: 13px; color: #666; margin-top: 4px; }
      .conf-bar-bg { height: 6px; background: #e5e5e0; border-radius: 3px; margin: 6px 0 2px; }
      .conf-bar    { height: 6px; background: #639922; border-radius: 3px; }
      .detail-row  { display: flex; justify-content: space-between; gap: 16px; padding: 7px 0;
                     border-bottom: 1px solid #f0f0ec; font-size: 13px; }
      .detail-row:last-child { border-bottom: none; }
      .detail-key  { color: #666; }
      .detail-val  { font-weight: 500; color: #1a1a1a; text-align: right; }
      .tag         { display: inline-block; font-size: 11px; padding: 2px 8px;
                     border-radius: 6px; font-weight: 500; }
      .tag-green   { background: #EAF3DE; color: #3B6D11; }
      .tag-amber   { background: #FAEEDA; color: #854F0B; }
      .tag-red     { background: #FCEBEB; color: #A32D2D; }
      .tag-blue    { background: #E6F1FB; color: #185FA5; }
      .upload-note { font-size: 12px; color: #777; margin-top: 4px; }
      .preview-grid { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 10px; }
      .preview-item { min-width: 0; }
      .preview-head { display: flex; align-items: center; justify-content: space-between;
                      gap: 8px; min-height: 18px; margin-bottom: 6px; font-size: 12px;
                      color: #555; }
      .preview-note { color: #999; font-size: 11px; }
      .preview-frame { aspect-ratio: 1 / 1; overflow: hidden; background: #f5f5f1;
                       border: 1px dashed #d8d8d0; border-radius: 8px; }
      .preview-frame .shiny-image-output { width: 100%; height: 100% !important; }
      .preview-frame img { width: 100%; height: 100%; object-fit: cover; display: block; }
      .btn-run     { width: 100%; background: #3B6D11; color: #fff; border: none;
                     border-radius: 8px; padding: 10px; font-size: 14px; font-weight: 500;
                     cursor: pointer; margin-top: 8px; }
      .btn-run:hover { background: #27500A; }
      .error-box   { background: #FCEBEB; border-radius: 8px; padding: 12px 16px;
                     font-size: 13px; color: #A32D2D; }
      @media (max-width: 767px) {
        .preview-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
    "))
  ),

  div(style = "max-width: 900px; margin: 32px auto; padding: 0 16px;",

    div(class = "main-title", "Fashion Match AI"),
    div(class = "sub-title",  "Upload a top, bottom and face photo to get your outfit compatibility score"),

    fluidRow(

      column(4,
        div(class = "panel",
          div(class = "panel-title", "Upload images"),
          fileInput("top_img",    "Top garment",    accept = c("image/jpeg", "image/png")),
          fileInput("bottom_img", "Bottom garment", accept = c("image/jpeg", "image/png")),
          fileInput("face_img",   "Face photo (optional)", accept = c("image/jpeg", "image/png")),
          div(class = "upload-note", "Face photo enables skin tone and face shape analysis")
        ),

        div(class = "panel",
          div(class = "panel-title", "Preview"),
          div(class = "preview-grid",
              preview_slot("Top", "top_preview"),
              preview_slot("Bottom", "bottom_preview"),
              preview_slot("Face", "face_preview", optional = TRUE))
        ),

        div(class = "panel",
          div(class = "panel-title", "Your details"),
          selectInput("mood", "Mood",
            choices  = c("casual", "professional", "romantic", "bold", "minimal",
                         "sporty", "elegant", "playful", "edgy", "classic"),
            selected = "casual"),
          selectInput("undertone", "Skin undertone",
            choices  = c("warm", "cool", "neutral"),
            selected = "warm"),
          selectInput("body_shape", "Body shape (optional)",
            choices  = c("not provided", "hourglass", "pear", "apple",
                         "rectangle", "inverted_triangle"),
            selected = "not provided"),
          tags$button("Analyse outfit", class = "btn-run",
                      onclick = "Shiny.setInputValue('run_btn', Math.random())")
        )
      ),

      column(8,
        uiOutput("results_ui")
      )
    )
  )
)
