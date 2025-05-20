colours <- c(
  "Waiting" = "#66CCEE",
  "Removed" = "#DDCC77",
  "Died" = "#BBBBBB",
  "Transplanted" = "#228833"
)
colours_trans <- alpha(colours, alpha = 0.5)

# factors considered text:
factors_not_included <- HTML("
<strong>Recipient BMI</strong> – Tested and not found to be significant in the model.<br><br>

<strong>Creatinine</strong> – The kidney function of donor is available but not
included. This is because it's not possible to know how many were on titration in
ITU. This would give a falsely low creatinine and potentially be misleading.<br><br>

<strong>Comorbidities (cardiovascular disease, vascular disease, stroke,
MI)</strong> – Not collected, looked into those that are, have a high proportion of
missing data.<br><br>

<strong>Time on dialysis</strong> – Recipient waiting time (years). Time waiting on
deceased donor kidney waiting list until time of transplant (active and suspended).
This can serve as a proxy for 'time on dialysis' as most patients are either already
on dialysis or due to commence dialysis within 6 months at the time of listing for
transplantation.")