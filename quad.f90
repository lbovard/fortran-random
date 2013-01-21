subroutine quad(len,a,b,c,x1,x2)
real(4) a(len),b(len), c(len), x1(len), x2(len), s
do i=1,len
s = b(i)**2 - 4.*a(i)*c(i)
if (s.ge.0.) then
x1(i) = sqrt(s)
x2(i) = (-x1(i) - b(i)) *0.5 / a(i)
x1(i) = ( x1(i) - b(i)) *0.5 / a(i)
else
x2(i)=0.
x1(i)=0.
endif
enddo
end

