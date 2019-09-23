//Tanner Lowthorp
//April 9, 2019
#include <iostream>
#include <vector>
#include <string>

using namespace std;

class BigInt{
    private:
        vector<char> storage;
    public:
        //Default constructor, creates BigInt with value of 0
        BigInt(){
            storage.push_back(0);
            storage.push_back(0);//sign decimal
        }
        //BigInt constructor converts int to BigInt
        BigInt(int x){
            int t = x;
            bool negative = (x < 0);
            x = abs(x);
            if(x == 0){
                storage.push_back(0);
                storage.push_back(0);//sign decimal
                return;
            }
            while(x > 0){
                storage.push_back(x % 10);
                x /= 10;
            }
            storage.push_back(0);
            if(negative) //sign decimal
                *this = tensComplement();
        }
        //BigInt constructor converts string to BigInt
        BigInt(string s){
            bool negative = (s[0] == '-');
            char c[s.length()];
            for(int i = s.length()-1; i >= negative; i--){
                c[i] = s[i] - 48;
                storage.push_back(c[i]);
            }
            storage.push_back(0);
            if(negative) //sign decimal
                *this = tensComplement();
            trim();
        }
        //Adds two BigInts digit by digit
        BigInt operator+(BigInt x){
            BigInt result;
            //Match number of digits
            while(storage.size() > x.storage.size())
                x.storage.push_back(x.storage[x.storage.size()-1]);
            while(storage.size() < x.storage.size())
                storage.push_back(storage[storage.size()-1]);

            result.storage.clear();
            int carry = 0;
            for(int i = 0; i < storage.size(); i++){
                int sum = storage[i] + x.storage[i] + carry;
                carry = sum / 10;
                result.storage.push_back(sum%10);
            }

            int temp = result.storage[result.storage.size()-1];
            if(temp == 1)
                result.storage.push_back(0);
            else if(temp == 8)
                result.storage.push_back(9);
            result.trim();
            trim();
            return result;
        }
        BigInt operator+(int x){
            return *this + BigInt(x);
        }
        friend BigInt operator+(int x, BigInt y){
            return y + x;
        }
        BigInt operator++(int){
            BigInt old = *this;
            *this = *this + 1;
            return old;
        }
        BigInt operator++(){
            *this += BigInt(1);
            return *this;
        }
        void operator+=(BigInt x){
            *this = *this + x;
        }
        void operator+=(int x){
            *this += BigInt(x);
        }
        //x-y = x + (-y)
        BigInt operator-(BigInt x){
            return *this + x.tensComplement();
        }
        BigInt operator-(int x){
            return *this - BigInt(x);
        }
        friend BigInt operator-(int x, BigInt& y){
            return y + ((-1) * x);
        }
        BigInt operator--(int){
            BigInt old = *this;
            *this = *this - 1;
            return old;
        }
        BigInt operator--(){
            *this -= BigInt(1);
            return *this;
        }
        void operator-=(BigInt x){
            *this = *this - x;
        }
        BigInt operator*(BigInt x){
            bool negative = true;
            BigInt temp = *this, result;
            if(storage[storage.size()-1] == x.storage[x.storage.size()-1])
                negative = false;
            if(isNegative())
                temp = tensComplement();
            if(x.isNegative())
                x = x.tensComplement();

            int i = 0;
            vector<BigInt> shortcuts, factors;
            shortcuts.push_back(temp);
            factors.push_back(BigInt(1));
            while(x >= factors[i]){
                shortcuts.push_back(shortcuts[i]+shortcuts[i]);
                factors.push_back(factors[i]+factors[i]);
                i++;
            }
            for(int j = shortcuts.size()-1; j >= 0; j--){
                if(x >= factors[j]){
                    result += shortcuts[j];
                    x -= factors[j];
                }
            }
            if(negative)
                return result.tensComplement();
            else
                return result;
        }
        BigInt operator*(int x){
            return *this * BigInt(x);
        }
        friend BigInt operator*(int x, BigInt y){
            return y * x;
        }
        void operator*=(BigInt x){
            *this = *this * x;
        }
        BigInt power(BigInt x){
            BigInt result(1);
            vector<BigInt> shortcuts,factors;
            shortcuts.push_back(*this);
            factors.push_back(BigInt(1));
            int i = 0;
            while(x >= factors[i] + factors[i]){
                shortcuts.push_back(shortcuts[i] * shortcuts[i]);
                factors.push_back(factors[i]+factors[i]);
                i++;
            }
            for(int j = shortcuts.size()-1; j >= 0; j--){
                if(x >= factors[j]){
                    result *= shortcuts[j];
                    x -= factors[j];
                }
            }
            return result;
        }
        BigInt operator/(BigInt x){
            bool negative = true;
            BigInt temp = *this, result;
            if(storage[storage.size()-1] == x.storage[x.storage.size()-1])
                negative = false;
            if(isNegative())
                temp = tensComplement();
            if(x.isNegative())
                x = x.tensComplement();

            int i = 0;
            vector<BigInt> shortcuts, factors;
            factors.push_back(1);
            shortcuts.push_back(x);
            while(temp >= (shortcuts[i]+shortcuts[i])){
                shortcuts.push_back(shortcuts[i] + shortcuts[i]);
                factors.push_back(factors[i] + factors[i]);
                i++;
            }
            for(int j = shortcuts.size()-1; j >= 0; j--){
                if(temp >= shortcuts[j]){
                    result += factors[j];
                    temp -= shortcuts[j];
                }
            }
            if(negative)
                return result.tensComplement();
            else
                return result;
        }
        BigInt operator/(int x){
            return *this / BigInt(x);
        }
        void operator/=(BigInt x){
            *this = *this / x;
        }
        BigInt operator%(BigInt x){
            bool negative = (storage[storage.size()-1] == 9);
            BigInt temp = *this, result;
            if(isNegative())
                temp = tensComplement();
            if(x.isNegative())
                x = x.tensComplement();

            int i = 0;
            vector<BigInt> shortcuts, factors;
            factors.push_back(BigInt(1));
            shortcuts.push_back(x);
            while(temp >= (shortcuts[i]+shortcuts[i])){
                shortcuts.push_back(shortcuts[i] + shortcuts[i]);
                factors.push_back(factors[i] + factors[i]);
                i++;
            }
            for(int j = shortcuts.size()-1; j >= 0; j--){
                if(temp >= shortcuts[j]){
                    result += factors[j];
                    temp -= shortcuts[j];
                }
            }
            if(negative)
                return temp.tensComplement();
            else
                return temp;
        }
        BigInt operator%(int x){
            return *this % BigInt(x);
        }
        void operator%=(BigInt x){
            *this = *this % x;
        }
        bool operator>(BigInt x){
            return !(*this - x).isNegative() && !(*this == x);
        }
        bool operator>(int x){
            return (*this > BigInt(x));
        }
        bool operator>=(BigInt x){
            return (*this > (x - 1));
        }
        bool operator==(BigInt x){
            BigInt comparable = *this - x;
            return comparable.storage == vector<char>{0,0};
        }
        bool operator<(BigInt x){
            return !(x - *this).isNegative() && !(*this == x);
        }
        bool operator<=(BigInt x){
            return (*this < (x + 1));
        }
        void showStorage(){
            cout << "Storage:";
            for(int i = storage.size()-1; i >= 0; i--)
                cout << (int) storage[i];
        }
        //Uses tens complement to convert to and from negative numbers
        BigInt tensComplement(){
            BigInt complement;
            complement.storage.clear();
            for(int i = 0; i < storage.size(); i++){
                complement.storage.push_back(9 - storage[i]);
            }
            complement++;
            return complement;
        }
        //removes unnecessary leading 0's or 9's
        void trim(){
            int s = storage.size();
            if(storage[s-1] == 9 && storage[s-2] == 9 && storage[s-3] == 0){
                for(int i = s-3; i >= 0; i--){
                    if(storage[i] != 0){
                        while(storage.size() > 2 && storage[storage.size()-1] == storage[storage.size()-2])
                            storage.pop_back();
                    }
                }
                return;
            }
            else{
                while(storage.size() > 2 && storage[storage.size()-1] == storage[storage.size()-2])
                    storage.pop_back();
            }
        }
        //0 if positive, 9 if negative
        bool isNegative() const{
            return storage[storage.size()-1];
        }
        friend ostream & operator<<(ostream & out, const BigInt & n){
            BigInt print = n;
            if(n.isNegative()){
                out << '-';
                print = print.tensComplement();
            }
            int i = n.storage.size()-2;
            int j = i - (i % 3) - 1;
            for(; i >= 0; i--){
                if(i == j){
                    out << ',';
                    j -= 3;
                }
                out << (int) print.storage[i];
            }
            return out;
        }
};
//factorial function for demonstration
BigInt fact(BigInt x){
    BigInt product(1);
    for(BigInt i(1); i <= x; i++){
        product *= i;
    }
    return product;
}

int main()
{
    cout << fact(500) << endl;
    return 0;
}
