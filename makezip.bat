SET ZIPDIR=%_CWD%
mkdir %TEMP%\adlib
del cmfplay.zip
copy /s *.* %TEMP%\adlib\
cd %TEMP%\adlib
del makezip.bat
rmcvsdir   %TEMP%\adlib
zip -r %ZIPDIR%\cmfplay.zip *.*
deltree /y %TEMP%\adlib
cd %ZIPDIR%
unset ZIPDIR
