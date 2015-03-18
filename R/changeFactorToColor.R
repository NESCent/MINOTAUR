  NeutCol <- factor(Is.Neut)
  levels(factor(Is.Neut))
  ### selected is first, neutral is second
  levels(NeutCol) <- c("orange", "black")
  ### orange is selected, black is neutral
  NeutCol <- as.character(NeutCol)