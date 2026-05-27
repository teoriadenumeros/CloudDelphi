# Generalized Cloud Model Development Toolkit for Delphi Studies (Multilingual)

An advanced mathematical and graphical framework written in **R** to process, synthesize, and visualize multi-expert data in **Delphi Studies** using **Generalized Cloud Models**. This toolkit models the subjective uncertainty, ambiguity, and fuzziness of expert panels across successive rounds, providing high-resolution control dashboards for academic and corporate decision-making.

## Key Features
* **Dynamic Expert Extraction:** Automatically calculates the panel size from linear data structures.
* **Split-Entropy Boundaries:** Mathematically models asymmetric cognitive structures (left and right uncertainties).
* **Multi-Round Evolution Tracking:** Monitors historical entropy variations and uncertainty indices across rounds.
* **High-Resolution Graphical Engine:** Automatically exports publication-ready dashboards (`.pdf`, `.tiff`, `.jpg`) with custom point-cloud iterations.
* **Native Multilingual Support:** Console outputs and chart labels dynamically adapt to English (`en`), Spanish (`es`), and Portuguese (`pt`).

---

## Data Input Architecture

To prevent structural overhead, each Delphi round is fed into the engine as a single, flat **continuous linear vector**. Data for each expert must be grouped sequentially in strict **4-value blocks** using the following order:

$$\text{Block}_i = [ \text{Minimum}, \text{Maximum}, \text{Crest } (Ex), \text{Confidence/Security} ]$$

* **Minimum:** Lower boundary of the expert's estimation interval.
* **Maximum:** Upper boundary of the expert's estimation interval.
* **Crest ($Ex$):** The most probable or ideal specific value according to the expert.
* **Confidence/Security:** A score bounded between $0$ and $1$ reflecting the expert's self-reported certainty level.

The core framework dynamically derives the total number of experts by dividing the vector's total length by 4:
$$\text{Number of Experts} = \frac{\text{Length of Vector}}{4}$$

---

## Quick Start & Implementation Example

The following script sets up a complete 3-round study with 7 experts showcasing extreme initial asymmetry evolving into a stable, well-balanced consensus.

```R
# 1. Load the Core Engine and Global Dictionary in your R environment
# (Paste the framework function definitions here)

# 2. Define your Empirical Dataset
# Round 1: Extreme initial divergence and highly skewed opinions
Round1_Data <- c(
  15, 75, 20, 0.85,   10, 60, 52, 0.05,   30, 90, 35, 0.75,  
  10, 50, 15, 0.60,   20, 85, 80, 0.85,   15, 70, 22, 0.02,  
  25, 80, 30, 0.70
)

# Round 2: Noticeable asymmetric convergence as intervals contract
Round2_Data <- c(
  25, 55, 30, 0.95,   35, 75, 42, 0.80,   30, 60, 34, 0.92,  
  28, 50, 32, 0.88,   32, 65, 36, 0.90,   40, 75, 45, 0.85,  
  28, 55, 33, 0.91
)

# Round 3: Stable final consensus with highly symmetrical balanced curves
Round3_Data <- c(
  30, 42, 36, 0.98,   34, 46, 40, 0.95,   32, 44, 38, 0.96,  
  31, 43, 37, 0.94,   33, 45, 39, 0.97,   35, 47, 41, 0.92,  
  30, 42, 36, 0.95
)

# 3. Bundle Rounds into a sequential list
MyResearchStudy <- list(Round_1 = Round1_Data, Round_2 = Round2_Data, Round_3 = Round3_Data)

# 4. Define historical tracking tracking vector (7 experts + 1 collective synthesis)
InitialEntropiesTrack <- c(4.0, 12.0, 3.5, 4.5, 5.0, 15.0, 4.0, 8.5)

# 5. Execute Evolution Framework
ResearchConsensus <- CloudDelphi_Evolution(
  RoundsList     = MyResearchStudy, 
  InitialEntropy = InitialEntropiesTrack, 
  Min            = 0,         # Minimum boundary of the Universe of Discourse
  Max            = 100,       # Maximum boundary of the Universe of Discourse
  Lang           = "es",      # Visual and console rendering language ("en", "es", "pt")
  DecimalPlaces  = 3          # Matrix precision format scale
)
