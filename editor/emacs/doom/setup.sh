# [[file:config.org::*Hunspell][Hunspell:1]]
cd /tmp
curl -o "hunspell-en-custom.zip" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=AU&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=hunspell'
unzip "hunspell-en-custom.zip"

sudo chown root:root en-custom.*
sudo mv en-custom.{aff,dic} /usr/share/myspell/
# Hunspell:1 ends here
