# StyleSync
An R Shiny app that scores outfit compatibility using colour harmony, skin tone matching, face shape detection, and body shape rules — no external APIs required.
A self-contained R project that analyses outfit compatibility using computer vision, unsupervised machine learning and rule-based style logic — no external APIs required.

## What it does

Upload a top, a bottom garment and optionally a face photo. The system returns a **compatibility score out of 100** based on:

- **Colour harmony** — HSV perceptual distance between top and bottom
- **Skin tone match** — Peer et al. skin pixel detection + ITU-R BT.601 luminance
- **Face shape + neckline** — OpenCV Haar cascade face detection, edge-based neckline classification
- **Silhouette fit** — edge gradient analysis of top and bottom shapes
- **Body shape rules** — flattering shape combinations per body type
- **Mood + undertone** — rule-based colour palette adjustments

## Quickstart

### 1. Install packages

```r
install.packages(c("imager", "shiny", "opencv"))
```

> Note: `opencv` may require the OpenCV system library. On Ubuntu: `sudo apt install libopencv-dev`

### 2. Launch the dashboard

```r
source("launch_dashboard.R")
launch_dashboard()
```

Or from the terminal:

```bash
Rscript launch_dashboard.R
```

### 3. Run tests

```r
source("test_run.R")
```

Place sample images in a `sample_images/` folder named `top.jpg`, `bottom.jpg`, `face.jpg` to run the full integration test.

## File structure

```
├── launch_dashboard.R      # Sources all modules + launches Shiny
├── ui.R                    # Shiny UI
├── server.R                # Shiny server
├── test_run.R              # Unit + integration tests
│
├── predict_outfit.R        # Main pipeline orchestrator
│
├── extract_color.R         # K-means dominant colour extraction
├── extract_skin_tone.R     # Skin pixel mask + luminance classification
├── detect_color_type.R     # 16-colour name classifier
├── color_harmony.R         # HSV perceptual distance score
├── color_temperature.R     # Warm / cool / neutral classification
├── pca_color_features.R    # PCA + elbow method on colour palette
├── cluster_style.R         # Silhouette-based k-means clustering
│
├── detect_top_shape.R      # Edge analysis: fitted / straight / flowy etc.
├── detect_bottom_shape.R   # Edge analysis: skinny / wide_leg / flared etc.
├── detect_face_shape.R     # OpenCV face crop + 5-region edge classification
├── detect_neckline.R       # Edge analysis on top 35% of garment image
│
├── undertone_match.R       # Warm / cool / neutral colour rule adjustments
├── mood_adjustment.R       # 8 mood × colour palette rules
├── neckline_face_match.R   # Neckline × face shape compatibility table
├── skin_match.R            # Skin tone × outfit colour bonus / penalty
└── body_shape_match.R      # Body shape × silhouette flattering rules
```

## How the score works

| Step | Contributes |
|---|---|
| Base colour harmony | Starting score (0–100) |
| Undertone match | ± adjustment |
| Mood adjustment | ± adjustment |
| Neckline × face shape | ± adjustment |
| Skin tone × outfit colour | ± adjustment |
| Body shape × silhouette | ± adjustment (capped at +20) |
| Final score | Clamped 0–100 |

## Tech stack

| Tool | Purpose |
|---|---|
| `imager` | Image loading, resizing, edge detection |
| `opencv` | Face detection (Haar cascade) |
| `base R` | `prcomp()` (PCA), `kmeans()`, `dist()` |
| `shiny` | Dashboard UI |

## Sample output

```
Compatibility Score : 78 / 100
Overall Confidence  : 0.8
Palette Style       : complementary
Top Shape           : fitted
Bottom Shape        : straight
Face Shape          : oval
Neckline            : v-neck
Top Colour Type     : blue
Bottom Colour Type  : black
Mood                : professional
Undertone           : cool
Body Shape          : hourglass
```
