;;; Compiled snippets and support files for `typescript-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'typescript-mode
                     '(("it" "it('${description}' () => {\n    // GIVEN\n    $0\n\n    // WHEN\n\n    // THEN\n\n});" "it in describe" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/test-it" nil nil)
                       ("expl" "expect(${object}).to.have.lengthOf($0);" "expect length" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/test-expect-length" nil nil)
                       ("expeq" "expect(${object}).to.equal($0);" "expect" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/test-expect-equal" nil nil)
                       ("expa" "expect(${object}).to.be.a('$0');" "expect to be a" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/test-expect-a" nil nil)
                       ("desc" "describe('${description}' () => {\n    $0\n});" "describe" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/test-describe" nil nil)
                       ("*@re" "* @return {${type}} - $0" "return comment" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/return-comment" nil nil)
                       ("ret" "return $0;" "return" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/return" nil nil)
                       ("*@pr" "* @property {${type}} ${name} - $0" "property comment" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/property-comment" nil nil)
                       ("*@pa" " * @param {${1:type}} ${2:name} - $0" "param comment" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/param-comment" nil nil)
                       ("/**" "/**\n * $0\n */" "multiline comment" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/multiline-comment" nil nil)
                       ("log" "console.log(`yas-selected-text`$0);" "log" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/log" nil nil)
                       ("let" "let ${name}: ${type} = $0" "let" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/let" nil nil)
                       ("int" "interface ${name} {\n    $0\n}" "interface" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/interface" nil nil)
                       ("impr" "import {$0} = require(\"${package}\");" "import require" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/import-require" nil nil)
                       ("impf" "import $2 from \"$1\";$0" "import from" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/import-from" nil nil)
                       ("ife" "if (${1:condition}) {\n   $0\n} else {\n}" "if else" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/ife" nil nil)
                       ("if" "if (${1:condition}) {\n   $0\n}" "if" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/if" nil nil)
                       ("fun" "function ${1:name}(${2:arg}: ${3:type}): ${res} {\n    $0\n}" "function" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/function" nil nil)
                       ("forof" "for(let ${1:item} of ${2:items}) {\n    $0\n}" "forof" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/forof" nil nil)
                       ("forin" "for(let ${1:item} in ${2:items}) {\n    $0\n}" "forin" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/forin" nil nil)
                       ("for" "for(var ${1:i} = ${2:0}; $1 < ${3:collection}.length; $1++) {\n    $0\n}" "for" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/for" nil nil)
                       ("enum" "enum ${name} {\n     $0\n}" "enum" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/enum" nil nil)
                       ("el" "else {\n    $0\n}" "else" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/el" nil nil)
                       ("const" "const ${name}: ${type} = $0" "constant" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/const" nil nil)
                       ("met" "${name}(${arg: ${type}}):${void} {\n    $0\n}" "class method" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/class-method" nil nil)
                       ("cons" "constructor(${x: ${type}}) {\n    $0\n}" "class constructor" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/class-constructor" nil nil)
                       ("class" "class ${name} {\n}" "class" nil nil nil "/home/vagrant/.emacs.d/snippets/typescript-mode/class" nil nil)))


;;; Do not edit! File generated at Fri Apr 13 18:37:17 2018
