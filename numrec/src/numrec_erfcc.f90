# 3 "numrec_erfcc.spp"
function numrec_erfcc_sgl(x) result(res)
  use numrec_kinds
  implicit none
  real(sgl)             :: res
  real(sgl), intent(in) :: x
  real(sgl) :: t,z
  z=abs(x)
  t=1.0_sgl/(1.0_sgl+0.5_sgl*z)
  res=t*exp(-z*z-1.26551223_sgl+t*(1.00002368_sgl+t*(.37409196_sgl+t* &
  (.09678418_sgl+t*(-.18628806_sgl+t*(.27886807_sgl+t*(-1.13520398_sgl+t* &
  (1.48851587_sgl+t*(-.82215223_sgl+t*.17087277_sgl)))))))))
  if (x<0.0_sgl) res=2.0_sgl-res
end function numrec_erfcc_sgl
# 3 "numrec_erfcc.spp"
function numrec_erfcc_dbl(x) result(res)
  use numrec_kinds
  implicit none
  real(dbl)             :: res
  real(dbl), intent(in) :: x
  real(dbl) :: t,z
  z=abs(x)
  t=1.0_dbl/(1.0_dbl+0.5_dbl*z)
  res=t*exp(-z*z-1.26551223_dbl+t*(1.00002368_dbl+t*(.37409196_dbl+t* &
  (.09678418_dbl+t*(-.18628806_dbl+t*(.27886807_dbl+t*(-1.13520398_dbl+t* &
  (1.48851587_dbl+t*(-.82215223_dbl+t*.17087277_dbl)))))))))
  if (x<0.0_dbl) res=2.0_dbl-res
end function numrec_erfcc_dbl
