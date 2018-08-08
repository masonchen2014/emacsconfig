# emacsconfig

1.First step :install godef and gocode using below commands:

  go get github.com/rogpeppe/godef
  
  go get github.com/nsf/gocode
  
2.Then put the init.el file into your ~/.emacs.d/ directory;

3.Replace the setting within the init.el with your own GoPATH and GoBIN:

    (setenv "GOPATH" "YOUR-OWN-GOPATH") 
    (add-to-list 'exec-path "YOUR-GO-BIN-PATH")

4.At last congratuations !!! You can write go-code using emacs now !!!
