## 🎈 Rancho Programming Language

## ⚙  How to Install it on (MAC)
------------------

* Install SWI-Prolog Version 7.6.4 ([Click to Install 🚀](https://www.swi-prolog.org/download/stable/bin/SWI-Prolog-7.6.4.dmg)) 

Note this does not work for latest SWI-Prolog for version 8 or above because [this](https://github.com/yuce/pyswip/issues/17)
* In your /etc/profile add these lines 
```
export PATH=$PATH:/Applications/SWI-Prolog.app/Contents/swipl/bin/x86_64-darwin15.6.0
export DYLD_FALLBACK_LIBRARY_PATH=/Applications/SWI-Prolog.app/Contents/swipl/lib/x86_64-darwin15.6.0
``` 
* Run main.py present in src
``` 
python3 main.py inputfile
``` 

