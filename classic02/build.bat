cls

wla-z80 -o test.o main.asm 

echo [objects] > linkfile
echo test.o >> linkfile

wlalink linkfile output.sms

::java -jar C:\SEGA\Emulicious\Emulicious.jar output.sms
output.sms