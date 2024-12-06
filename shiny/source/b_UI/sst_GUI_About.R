# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 01182024
#                   Header: 🗸  Comments:🗸   Refactored: 🗸          
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: sst_GUI_About.
#
#                               DESCRIPTION
# This program defines the UI for the About tab.
# 
#                             TABLE OF CONTENTS
#       1) MAIN LAYOUT
#
#
# ==============================================================================
# ==============================================================================
#
#                             1) MAIN LAYOUT
#
# ==============================================================================
# Export tab UI elements are defined here. 
# Overall tabPanel defined in source/a_StartHere/appUI.R. 
About_GUITAB <- tabPanel(
  "About",
  mainPanel(
    shinyjs::useShinyjs(),
    id = "aboutPanel", width = 12,

    div(
      HTML("Primary Author:<br>Stephen Tennyson<br>Stephen.tennyson@nist.gov<br>
      
      <br>Theory Points of Contact:<br>Paul Patrone<br>Paul.patrone@nist.gov<br>
      
      <br><br>Anthony Kearsley<br>Anthony.kearsley@nist.gov<br>
      
      <br> This work is a product of the Mathematics of Applied Diagnostics 
      (MAD) Project in the NIST Applied & Computational Mathematics Division 
      (Math Modeling Group).<br><br>
      <br>
      Theory References:
      <br>
      [1] P. N. Patrone, A. J. Kearsley, Classification under uncertainty: data 
      analysis for diagnostic antibody testing, Mathematical Medicine and 
      Biology: A Journal of the IMA 38 (3) (2021) 396–416. 
      doi: 10.1093/imammb/dqab007.
      <br>
      https://academic.oup.com/imammb/article/38/3/396/6336022<br>
      <br>
      [2] P. N. Patrone, A. J. Kearsley, Minimizing uncertainty in prevalence 
      estimates, Statistics & Probability Letters 205 (2024) 109946. 
      doi: https://doi.org/10.1016/j.spl.2023.109946.
      <br>
      https://www.sciencedirect.com/science/article/pii/S0167715223001700
      <br>
      [3] P. N. Patrone, P. Bedekar, N. Pisanic, Y. C. Manabe, D. L. Thomas, C.
      D. Heaney, A. J. Kearsley, Optimal decision theory for diagnostic testing:
      Minimizing indeterminate classes with applications to saliva-based 
      sars-cov-2 antibody assays, Mathematical Biosciences 351 (2022) 108858. 
      doi: https://doi.org/10.1016/j.mbs.2022.108858.
      <br>
      https://www.sciencedirect.com/science/article/pii/S0025556422000608?via%3Dihub
      <br>
      [4] P. N. Patrone, R. A. Binder, C. S. Forconi, A. M. Moormann, A. J. 
      Kearsley, Minimal Assumptions for Optimal Serology Classification: Theory 
      and Implications for Multidimensional Settings and Impure Training Data,
      Arxiv (2023) 2309.00645, doi: https://doi.org/10.48550/arXiv.2309.00645.
      <br>
      https://arxiv.org/abs/2309.00645
      <br>
      <br>
      Implementation References: 
      <br>
      R Core Team (2021). R: A language and environment for statistical
      computing. R Foundation for Statistical Computing, Vienna, Austria.
      <br>
      URL https://www.R-project.org/.
      <br>
      Nocedal, J. and Wright, S. J. (1999). Numerical Optimization. Springer.
      <br>
      <br>
      Disclaimers: 
      <br>
      THE SOFTWARE IS PROVIDED 'AS IS' WITHOUT ANY WARRANTY OF ANY KIND, EITHER
      EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY 
      WARRANTY THAT THE SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED 
      WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND 
      FREEDOM FROM INFRINGEMENT, AND ANY WARRANTY THAT THE DOCUMENTATION WILL 
      CONFORM TO THE SOFTWARE, OR ANY WARRANTY THAT THE SOFTWARE WILL BE ERROR
      FREE. IN NO EVENT SHALL NIST BE LIABLE FOR ANY DAMAGES, INCLUDING, BUT 
      NOT LIMITED TO, DIRECT, INDIRECT, SPECIAL OR CONSEQUENTIAL DAMAGES, 
      ARISING OUT OF, RESULTING FROM, OR IN ANY WAY CONNECTED WITH THIS 
      SOFTWARE, WHETHER OR NOT BASED UPON WARRANTY, CONTRACT, TORT, OR 
      OTHERWISE, WHETHER OR NOT INJURY WAS SUSTAINED BY PERSONS OR PROPERTY OR 
      OTHERWISE, AND WHETHER OR NOT LOSS WAS SUSTAINED FROM, OR AROSE OUT OF THE
      RESULTS OF, OR USE OF, THE SOFTWARE OR SERVICES PROVIDED HEREUNDER.
      <br>
      This software was developed by employees of the National
         Institute of Standards and Technology (NIST), an agency of the Federal
         Government and is being made available as a public service. Pursuant
         to title 17 United States Code Section 105, works of NIST employees
         are not subject to copyright protection in the United States.  This
         software may be subject to foreign copyright.  Permission in the
         United States and in foreign countries, to the extent that NIST may
         hold copyright, to use, copy, modify, create derivative works, and
         distribute this software and its documentation without fee is hereby
         granted on a non-exclusive basis, provided that this notice and
         disclaimer of warranty appears in all copies
<br>
           ")
    )
  ) # end mainPanel
) # end tabPanel
