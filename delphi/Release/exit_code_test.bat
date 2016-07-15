@FastTest.exe -go -reset -fast CP210xManufacturing.dll -exit
@if ERRORLEVEL 1 goto PringError 
@if ERRORLEVEL 0 goto PringSuccessful

:PringError
@echo Error
@goto EXIT

:PringSuccessful
@echo Successful
@goto EXIT

:EXIT