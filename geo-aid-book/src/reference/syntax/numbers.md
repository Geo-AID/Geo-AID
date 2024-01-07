# Numbers

> <sup>**Lexer**</sup>\
> NUMBER :\
> &nbsp;&nbsp; &nbsp;&nbsp; INTEGER\
> &nbsp;&nbsp; | FLOAT\
> \
> INTEGER :\
> &nbsp;&nbsp; Digit<sup>+</sup>
> \
> FLOAT :\
> &nbsp;&nbsp; INTEGER `.` Digit<sup>*</sup>

Where *Digit* is an ASCII digit (`0-9`). Either integers or decimals.