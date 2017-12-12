# Install Pip
declare -a apt=("python3-pip" "libicu-dev")

for i in "${apt[@]}"
do
    sudo apt-get -y install "$i"
done

# Install dependencies
declare -a pip=("numpy" "sklearn" "polyglot" "nltk" "scipy")

for i in "${pip[@]}"
do
    sudo pip3 install "$i"
done
