# ==============================================================================
# GENERALIZED CLOUD MODEL DEVELOPMENT TOOLKIT FOR DELPHI STUDIES (MULTILINGUAL)
# ==============================================================================
# Copyright (c) 2026 Miguel Cruz Ramírez & Antonio Nariño University (Bogotá)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# ==============================================================================
# DATA INPUT DYNAMICS (Expert Vector Structure):
#   Each Delphi round must be structured as a continuous linear vector where 
#   individual expert responses are sequentially grouped in 4-value blocks 
#   in the following strict order:
#                  [Minimum, Maximum, Crest (Ex), Confidence/Security].
#   The framework dynamically computes the total number of experts by dividing 
#   the total vector length by 4. It is highly recommended to execute two or 
#   three rounds at most. The core system natively supports three languages: 
#   Spanish ("es"), English ("en"), and Portuguese ("pt"). If an unindexed 
#   language code is requested, the system safely falls back to English 
#   to prevent fatal runtime disruptions.
# ==============================================================================

# GLOBAL TRANSLATION DICTIONARY
.CloudDelphi_I18N <- list(
  es = list(
    error_empty   = "Error: La lista de rondas (RoundsList) no puede estar vacía.",
    success_msg   = "ÉXITO: ¡El panel de control del Framework Cloud Delphi se ha exportado correctamente!",
    metrics_round = ">>> MÉTRICAS PARA LA RONDA %d <<<",
    univ_dis_x    = "Universo de Discurso (x)",
    cert_degree   = "Grado de Certeza (μ)",
    delphi_eval   = "Evaluación Delphi: Ronda %d",
    group_stats   = "Métricas de Consenso Grupal:\n - Cresta (Ex): %s\n - Ambigüedad (En): %s\n - Borrosidad (He): %s",
    leg_indiv     = "Nubes de Expertos Individuales (Empírico)",
    leg_synth     = "Nube Sintética Colectiva (Gotas Grupales)",
    leg_profile   = "Perfil Esperado Colectivo (Modelo Matemático)"
  ),
  en = list(
    error_empty   = "Error: The RoundsList cannot be empty.",
    success_msg   = "SUCCESS: Unified Cloud Delphi Framework Dashboard Successfully Exported!",
    metrics_round = ">>> METRICS FOR ROUND %d <<<",
    univ_dis_x    = "Universe of Discourse (x)",
    cert_degree   = "Certainty Degree (μ)",
    delphi_eval   = "Delphi Evaluation: Round %d",
    group_stats   = "Group Consensus Metrics:\n - Crest (Ex): %s\n - Ambiguity (En): %s\n - Blurriness (He): %s",
    leg_indiv     = "Individual Expert Clouds (Empirical)",
    leg_synth     = "Collective Synthetic Cloud (Group Drops)",
    leg_profile   = "Collective Expected Profile (Mathematical Model)"
  ),
  pt = list(
    error_empty   = "Erro: A lista de rodadas (RoundsList) não pode estar vazia.",
    success_msg   = "SUCESSO: Painel de Controle do Framework Cloud Delphi Exportado com Sucesso!",
    metrics_round = ">>> MÉTRICAS PARA A RODADA %d <<<",
    univ_dis_x    = "Universo de Discurso (x)",
    cert_degree   = "Grau de Certeza (μ)",
    delphi_eval   = "Avaliação Delphi: Rodada %d",
    group_stats   = "Métricas de Consenso Grupal:\n - Crista (Ex): %s\n - Ambiguidade (En): %s\n - Imprecisão (He): %s",
    leg_indiv     = "Nuvens de Especialistas Individuais (Empírico)",
    leg_synth     = "Nuvem Sintética Coletiva (Gotas Grupais)",
    leg_profile   = "Perfil Esperado Coletivo (Modelo Matemático)"
  )
)

# ==============================================================================
# 1. MATHEMATICAL CORE ENGINE (Generalized Multi-Expert Operator)
# ==============================================================================
CloudDelphi_Core = function(CurrentData, ForegoingData = NULL, ForegoingEntropy = NULL,
                            Min = 0, Max = 100) {
  Experts <- length(CurrentData) / 4
  
  Ex_i  <- numeric(Experts) 
  En_L  <- numeric(Experts) 
  En_R  <- numeric(Experts) 
  He_i  <- numeric(Experts) 
  Conf_i <- numeric(Experts) 
  
  for (i in 1:Experts) {
    idx <- (i - 1) * 4 + 1
    ex_min  <- CurrentData[idx]
    ex_max  <- CurrentData[idx + 1]
    ex_prob <- CurrentData[idx + 2]
    ex_sec  <- CurrentData[idx + 3]
    
    Ex_i[i]   <- ex_prob
    Conf_i[i] <- ex_sec
    
    Insecurity_Factor <- 1 - ex_sec
    
    # Structural Calculation of Split-Entropy Boundaries
    En_L[i] <- ((ex_prob - ex_min) * (1 + Insecurity_Factor)) / 3
    En_R[i] <- ((ex_max - ex_prob) * (1 + Insecurity_Factor)) / 3
    
    En_L[i] <- max(En_L[i], 0.001)
    En_R[i] <- max(En_R[i], 0.001)
  }
  
  if (is.null(ForegoingData) || length(ForegoingData) == 0) {
    for (i in 1:Experts) {
      Imbalance_Factor <- abs(En_L[i] - En_R[i]) / (En_L[i] + En_R[i] + 0.01)
      K_Security <- 3 + (3 * Conf_i[i])
      K_Dynamic  <- K_Security * (1 - (0.5 * Imbalance_Factor))
      K_Dynamic  <- max(K_Dynamic, 2.0)
      
      He_i[i] <- (En_L[i] + En_R[i]) / (2 * K_Dynamic)
    }
  } else {
    for (i in 1:Experts) {
      idx <- (i - 1) * 4 + 1
      var_min  <- abs(CurrentData[idx] - ForegoingData[idx])
      var_max  <- abs(CurrentData[idx + 1] - ForegoingData[idx + 1])
      var_prob <- abs(CurrentData[idx + 2] - ForegoingData[idx + 2])
      He_i[i]  <- (var_min + var_max + var_prob) / 18
    }
  }
  
  # Synthesis Matrix Generation (Collective Expected Properties)
  Ex_s  <- mean(Ex_i)
  En_Ls <- (Ex_s - min(Ex_i - 3 * En_L)) / 3
  En_Rs <- (max(Ex_i + 3 * En_R) - Ex_s) / 6
  He_s  <- mean(He_i)
  
  Weight <- numeric(Experts)
  for (i in 1:Experts) {
    Weight[i] <- Conf_i[i] / (abs((Ex_i[i] - Ex_s) / Ex_s) + ((En_L[i] + En_R[i])/2) + He_i[i] + 0.01)
  }
  Weight <- Weight / sum(Weight)
  
  En_MeanCurrent <- (En_L + En_R) / 2
  DeltaEntropyIndex <- numeric(Experts + 1)
  if (!is.null(ForegoingEntropy) && length(ForegoingEntropy) != 0) {
    for (i in 1:Experts) {
      DeltaEntropyIndex[i] <- abs(ForegoingEntropy[i] - En_MeanCurrent[i]) / max(ForegoingEntropy[i], 0.01)
    }
    En_MeanSynthetic <- (En_Ls + En_Rs) / 2
    DeltaEntropyIndex[Experts + 1] <- abs(ForegoingEntropy[Experts + 1] - En_MeanSynthetic) / max(ForegoingEntropy[Experts + 1], 0.01)
  }
  
  UncertaintyIndex <- numeric(Experts + 1)
  UncertaintyIndex[1:Experts] <- He_i / En_MeanCurrent
  UncertaintyIndex[Experts + 1] <- He_s / ((En_Ls + En_Rs) / 2)
  
  Round <- cbind(
    Ex_Prob   = c(Ex_i, Ex_s),
    En_Left   = c(En_L, En_Ls),
    En_Right  = c(En_R, En_Rs),
    He_Hyper  = c(He_i, He_s),
    Weight    = c(Weight, 1.00),
    Delta_En  = DeltaEntropyIndex,
    Unc_Index = UncertaintyIndex
  )
  
  dimnames(Round)[[1]] <- paste0("Exp_", c(1:Experts, "s"))
  return(t(Round))
}

# ==============================================================================
# 2. GLOBAL MANAGER & HIGH-RESOLUTION DASHBOARD GENERATOR
# ==============================================================================
CloudDelphi_Evolution = function(RoundsList, InitialEntropy = NULL,
                                 Min = 0, Max = 100, Lang = "en",
                                 CloudExpertIteration = 1200,   
                                 CloudSyntecticIteration = 2500, 
                                 DecimalPlaces = 2) {
  
  if (!(Lang %in% names(.CloudDelphi_I18N))) Lang <- "en"
  TXT <- .CloudDelphi_I18N[[Lang]]
  
  TotalRounds <- length(RoundsList)
  if (TotalRounds == 0) stop(TXT$error_empty, call. = FALSE)
  
  UserPath <- Sys.getenv("HOME")
  if (.Platform$OS.type == "windows") UserPath <- Sys.getenv("USERPROFILE")
  BaseFolder <- file.path(UserPath, "Documents")
  
  TiffPath <- file.path(BaseFolder, "CloudDelphi_Evolution_Analysis.tiff")
  JpgPath  <- file.path(BaseFolder, "CloudDelphi_Evolution_Analysis.jpg")
  PdfPath  <- file.path(BaseFolder, "CloudDelphi_Evolution_Analysis.pdf")
  
  ResultsHistory <- list()
  for (r in 1:TotalRounds) {
    CurrentData <- RoundsList[[r]]
    if (r == 1) {
      ForegoingData <- NULL
      ForegoingEntropy <- InitialEntropy
    } else {
      ForegoingData <- RoundsList[[r - 1]]
      ForegoingEntropy <- (ResultsHistory[[r - 1]]["En_Left", ] + ResultsHistory[[r - 1]]["En_Right", ]) / 2
    }
    ResultsHistory[[r]] <- CloudDelphi_Core(CurrentData, ForegoingData, ForegoingEntropy, Min, Max)
  }
  
  GenerarPanelGlobal <- function() {
    old_par <- par(oma = c(6, 1, 1, 1), mar = c(4.5, 5, 4, 1.5), cex.lab = 1.4, cex.axis = 1.3)
    on.exit(par(old_par))
    layout(matrix(1:TotalRounds, nrow = 1, ncol = TotalRounds))
    
    for (r in 1:TotalRounds) {
      CurrentData <- RoundsList[[r]]
      RoundMatrix <- ResultsHistory[[r]]
      Experts <- length(CurrentData) / 4
      
      plot(0, 0, type = "n", ylim = c(-0.02, 1.12), xlim = c(Min, Max),
           xlab = TXT$univ_dis_x, 
           ylab = TXT$cert_degree, 
           main = sprintf(TXT$delphi_eval, r), cex.main = 1.6)
      
      DrawCloudSpace = function(Ex, En_L, En_R, He, Color, Size, N, IsSynthetic = FALSE) {
        X_i <- numeric(N)
        Mu_i <- numeric(N)
        for (j in 1:N) {
          En_Side <- ifelse(runif(1) > 0.5, En_R, En_L)
          En_Rand <- rnorm(1, mean = En_Side, sd = He)
          if(En_Rand <= 0) En_Rand <- 0.01 
          
          X_val   <- rnorm(1, mean = Ex, sd = En_Rand)
          X_i[j]  <- X_val
          Mu_i[j] <- exp(-((X_val - Ex)^2) / (2 * En_Rand^2))
        }
        points(X_i, Mu_i, col = Color, pch = ifelse(IsSynthetic, 4, 20), cex = Size)
        if (IsSynthetic) {
          curve(exp(-((x - Ex)^2) / (2 * ifelse(x > Ex, En_R, En_L)^2)), 
                add = TRUE, col = "black", lwd = 3)
        }
      }
      
      for (i in 1:Experts) {
        DrawCloudSpace(RoundMatrix["Ex_Prob", i], RoundMatrix["En_Left", i], 
                       RoundMatrix["En_Right", i], RoundMatrix["He_Hyper", i], 
                       "grey65", 0.35, CloudExpertIteration, IsSynthetic = FALSE)
      }
      
      DrawCloudSpace(RoundMatrix["Ex_Prob", "Exp_s"], RoundMatrix["En_Left", "Exp_s"], 
                     RoundMatrix["En_Right", "Exp_s"], RoundMatrix["He_Hyper", "Exp_s"], 
                     "black", 0.6, CloudSyntecticIteration, IsSynthetic = TRUE)
      
      text(RoundMatrix["Ex_Prob", "Exp_s"], 1.10, "Exp_s", col = "black", cex = 1.2, font = 2)
      for (i in 1:Experts) {
        Y_Offset <- ifelse(i %% 2 == 0, 1.03, 1.06)
        text(RoundMatrix["Ex_Prob", i], Y_Offset, colnames(RoundMatrix)[i], col = "grey30", cex = 0.9)
      }
      
      synth_ex  <- round(RoundMatrix["Ex_Prob", "Exp_s"], DecimalPlaces)
      synth_en  <- round((RoundMatrix["En_Left", "Exp_s"] + RoundMatrix["En_Right", "Exp_s"]) / 2, DecimalPlaces)
      synth_he  <- round(RoundMatrix["He_Hyper", "Exp_s"], DecimalPlaces)
      
      stats_text <- sprintf(TXT$group_stats, format(synth_ex, nsmall=DecimalPlaces), 
                            format(synth_en, nsmall=DecimalPlaces), format(synth_he, nsmall=DecimalPlaces))
      
      TargetX <- Min + (Max - Min) * 0.72
      TargetY = 0.65
      
      legend(x = TargetX, y = TargetY, legend = stats_text, bty = "o", bg = "#F8F9FA", 
             box.col = "grey75", cex = 1.1, adj = c(0, 0.2))
    }
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    
    # Enhanced label spacing to prevent overlapping on horizontal layouts
    legend("bottom", 
           legend = c(TXT$leg_indiv, TXT$leg_synth, TXT$leg_profile),
           col = c("grey62", "black", "black"), 
           lty = c(0, 0, 1), 
           lwd = c(0, 0, 3),
           text.width = c(26, 26, 26), 
           pch = c(20, 4, NA), 
           pt.cex = c(1.5, 1.4, 0), 
           pt.lwd = c(1, 3, 1), 
           bty = "n", horiz = TRUE, cex = 1.2)
  }
  
  tiff(filename = TiffPath, width = 24, height = 9, units = "in", res = 300)
  GenerarPanelGlobal()
  dev.off()
  
  jpeg(filename = JpgPath, width = 24, height = 9, units = "in", res = 300, quality = 96)
  GenerarPanelGlobal()
  dev.off()
  
  pdf(file = PdfPath, width = 24, height = 9)
  GenerarPanelGlobal()
  dev.off()
  
  cat("\n======================================================================\n")
  cat("", TXT$success_msg, "\n")
  cat("======================================================================\n")
  
  NamedResults <- list()
  for (r in 1:TotalRounds) {
    cat(paste0("\n", sprintf(TXT$metrics_round, r), "\n"))
    FormattedMatrix <- round(ResultsHistory[[r]], DecimalPlaces)
    print(FormattedMatrix)
    cat("----------------------------------------------------------------------\n")
    NamedResults[[paste0("Round_", r)]] <- FormattedMatrix
  }
  
  return(NamedResults)
}

# ==============================================================================
# 3. EMPIRICAL CASE SIMULATION (3-ROUND ANISOTROPIC CONVERGENCE ANALYSIS)
# ==============================================================================

# Round 1: Extreme asymmetry. 
# Notice how the crests (3rd values) are highly skewed towards either the mins or maxes.
# E.g., Exp_1: Range [15, 75] with Crest at 20 (Strong positive/right skewness)
# E.g., Exp_5: Range [20, 85] with Crest at 80 (Strong negative/left skewness)
Round1_Data <- c(
  15, 75, 20, 0.85,   10, 60, 52, 0.05,   30, 90, 35, 0.75,  
  10, 50, 15, 0.60,   20, 85, 80, 0.85,   15, 70, 22, 0.02,  
  25, 80, 30, 0.70
)

# Round 2: Moderate but noticeable asymmetry.
# Experts begin contracting their estimation intervals but still share a noticeable bias.
# Most peaks intentionally lean towards the lower limit of the discourse space.
Round2_Data <- c(
  25, 55, 30, 0.95,   35, 75, 42, 0.80,   30, 60, 34, 0.92,  
  28, 50, 32, 0.88,   32, 65, 36, 0.90,   40, 75, 45, 0.85,  
  28, 55, 33, 0.91
)

# Round 3: Final structural consensus and system equilibrium.
# Crests are positioned precisely at the midpoint of individual expert ranges.
# This convergence forces the final round cloud graphs to look perfectly symmetrical.
Round3_Data <- c(
  30, 42, 36, 0.98,   34, 46, 40, 0.95,   32, 44, 38, 0.96,  
  31, 43, 37, 0.94,   33, 45, 39, 0.97,   35, 47, 41, 0.92,  
  30, 42, 36, 0.95
)

# Bundle sequential rounds into an execution list
DelphiStudyRounds   <- list(Round_1 = Round1_Data, Round_2 = Round2_Data, Round_3 = Round3_Data)

# Historical tracker entropy baseline vector (7 individual experts + 1 unified synthesis)
InitialHistoricalEntropies <- c(4.0, 12.0, 3.5, 4.5, 5.0, 15.0, 4.0, 8.5)

# Execute the runtime environment
ResearchConsensus <- CloudDelphi_Evolution(
  RoundsList     = DelphiStudyRounds, 
  InitialEntropy = InitialHistoricalEntropies, 
  Min            = 0,         # Universal Discourse Space lower bound
  Max            = 100,       # Universal Discourse Space upper bound
  Lang           = "en",      # System output localization rendering ("en", "es", "pt")
  DecimalPlaces  = 3          # Console print precision matrix formatting scale
)

