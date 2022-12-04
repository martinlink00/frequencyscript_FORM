{

rm params.txt
echo "3" > params.txt
math12 -script freq.wls
rm params.txt
echo "2" > params.txt
math12 -script freq.wls
rm params.txt
echo "4" > params.txt
math12 -script freq.wls

# Try
} || {
# Executed when above fails
rm params.txt
echo "3" > params.txt
math -script freq.wls
rm params.txt
echo "2" > params.txt
math -script freq.wls
rm params.txt
echo "4" > params.txt
math -script freq.wls

}
