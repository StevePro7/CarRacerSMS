cls

if exist test.o del test.o

wla-z80 -o test.o main.asm 

echo [objects] > linkfile
echo test.o >> linkfile

wlalink linkfile output.sms

if exist output.sms.sym del output.sms.sym
if exist test.o del test.o

::java -jar C:\SEGA\Emulicious\Emulicious.jar output.sms
output.sms