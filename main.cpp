#include<bits/stdc++.h>
#include <iostream>
#include<string>
using namespace std;

#define pb push_back
#define mp make_pair
#define s second
#define f first
#define MOD 998244353
#define lp(i,n) for(int i = 0;i < n;++i)
#define sz(x) ((int)(x).size())



typedef  long long ll;
typedef pair<ll , ll> pii;
typedef  double ld;
typedef unsigned long long ull ;
static const int OO = 0x3f3f3f3f , N =3*1e3+10 ;
static const long long OOL = 1e15+1LL ;

int dx[] = {1 , 1 , 2 , 2 , 3 , 3 };
int dy[] = {2 , 3 , 1 , 3 , 1 , 2 };

int changes,steps=1;
double EPSILON = 0.00001;
const long precision = 1000000000; // This is the accuracy.

class Fraction{
public:
    long num=0, den=1;
    double value=0;
    int whole=0;
    bool negative=false;
    string input;

    void simplify(){
        while(__gcd(num,den)!=1){
            int gcd = __gcd(num,den);
            num/=gcd;
            den/=gcd;
        }

        while(num>=den)num-=den,whole++,value+=1.0;
        value+=(double)((double)num/(double)den);
    }
    void bs_fraction(){
        while(whole>0)num+=den,whole--;

        value=(double)((double)num/(double)den);
    }
    void check_negativity(Fraction f2, Fraction &result){
        if((*this).negative && f2.negative)return;
        if((*this).negative || f2.negative)result.negative=true;
    }
    Fraction operator+(Fraction f2){
        Fraction addition;
        bs_fraction();
        f2.bs_fraction();
        addition.num=(negative?-1:1)*(num*f2.den)+(den*f2.num)*(f2.negative?-1:1);
        if(addition.num<0)addition.num*=-1,addition.negative=true;
        addition.den = den*f2.den;
        addition.simplify();
        (*this).simplify();
          f2.simplify();

        return addition;
    }
    Fraction operator*(Fraction f2){
        Fraction mul;
        (*this).bs_fraction();
        f2.bs_fraction();
       // cout << num << " " << den << " " << f2.num << " " << f2.den << " " << whole << endl;
        mul.num=(num*f2.num);
        mul.den = (den*f2.den);
        (*this).simplify();
          f2.simplify();
        mul.simplify();
        check_negativity(f2,mul);
        return mul;
    }
    Fraction operator-(Fraction f2){
        Fraction subtraction;
        bs_fraction();
        f2.bs_fraction();
        subtraction.num=(negative?-1:1)*(num*f2.den)-(den*f2.num)*(f2.negative?-1:1);
        if(subtraction.num<0)subtraction.num*=-1,subtraction.negative=true;
        subtraction.den = den*f2.den;
        (*this).simplify();
          f2.simplify();
        subtraction.simplify();

        return subtraction;
    }

    Fraction operator-(){

        (*this).negative=1-negative;

        return (*this);

    }
     Fraction operator/(Fraction f2){
        Fraction div;
        bs_fraction();
        f2.bs_fraction();
        div.num=(num*f2.den);
        div.den = (den*f2.num);
        (*this).simplify();
          f2.simplify();
        div.simplify();
        check_negativity(f2,div);
        return div;
    }

    bool operator==(Fraction f2){
        if(fabs(value - f2.value) < EPSILON)
            return true;
        return false;
    }
    bool operator<(Fraction f2){
        if(value < f2.value)
            return true;
        return false;
    }
    bool operator>(Fraction f2){
        if(value > f2.value)
            return true;
        return false;
    }
    operator int(){
        return (int)value;
    }
    operator double(){
        return value;
    }

     Fraction operator =(int i){
        Fraction f;
        num=i,den=1;
        f.simplify();
        return (*this);
    }
    Fraction operator=(double d){
    Fraction f;
    double integral = floor(d);
    double frac = d - integral;

    long garb = frac*precision;
    (*this).den = precision;
    (*this).num= garb;
    num+=den*integral;
     (*this).simplify();
    return (*this);
    }
};


ostream & operator << (ostream &out,  Fraction &f)
{
     f.simplify();
     if(f.negative)out<<'-';
    if(f.whole)out<<f.whole;
    if(f.num)
    out<<(f.whole?".":"")<<f.num;
    if(f.den != 1)out<<"/"<<f.den;
    return out;
}

istream & operator >> (istream &in,  Fraction &f)
{
    in>>f.input;

    if(find(f.input.begin(),f.input.end(),'/')!=f.input.end() || find(f.input.begin(),f.input.end(),'.') == f.input.end()){
            f.input+=' ';
    lp(i,sz(f.input)){
        if(f.input[i] == '-')f.negative=true;
        if(f.input[i] == '.' && i-1 >= 0 || f.input[i] == ' '){
                 int start=i,w=0;
        while(start>0&&f.input[start-1]<='9'&&f.input[start-1]>='0')start--;
        while(start<i)w*=10,w+=f.input[start]-'0',start++;
                f.whole=w;
        f.value=w;
        }
        if(f.input[i] == '/'){
            int start=i,n=0,d=0;
        while(start>0&&f.input[start-1]<='9'&&f.input[start-1]>='0')start--;
        while(start<i)n*=10,n+=f.input[start]-'0',start++;
        start++;
        while(start<sz(f.input) && f.input[start]<='9' && f.input[start]>='0')d*=10,d+=f.input[start]-'0',start++;
            f.num=n;
            f.den=d;
            if(!f.den)f.den=1;
            break;
        }


    }}
    else {
      int start=0;
        double n=0,d=0;
        for(int i = 0;i < sz(f.input);++i)if(f.input[i] == '.'){start=i;break;}
        start++;
        while(start>0&&f.input[start-1]<='9'&&f.input[start-1]>='0')start--;
        while(start<sz(f.input))n*=10,n+=f.input[start]-'0',start++;
        f=(double)(n/10.0);

    }
    f.simplify();
    return in;
}
template<typename T>
class Matrix
{
public:

    int row , column;
    T* matrix;
    T initial;
    Matrix(int n=0 ,int m=0)
    {
        row = n,column = m;
        matrix = new T[n*m];

    }

    Matrix<T> operator=(Matrix<T> m2)
    {
        matrix=m2.matrix;
        return m2;
    }

    Matrix<T> operator+(Matrix<T> m2)
    {
        T *sum = new T[m2.row*m2.column];
        lp(i,m2.row)lp(j,m2.column)sum[i*m2.column+j]=matrix[i*m2.column+j]+m2.matrix[i*m2.column+j];
        m2.matrix=sum;
        return m2;
    }


    Matrix<T> operator-(Matrix<T> m2)
    {
        T *diff = new T[m2.row*m2.column];
        lp(i,m2.row)
        lp(j,m2.column)
        diff[i*m2.column+j]=matrix[i*m2.column+j]-m2.matrix[i*m2.column+j];
        m2.matrix=diff;
        return m2;
    }

    Matrix<T> operator*(Matrix<T> m2)
    {
        T *mul = new T[row*m2.column];
        for(int i = 0; i < row; ++i)
            for(int j = 0; j < m2.column; ++j)
            {
                mul[i*m2.column+j]=(*this).initial;
            }
        for(int i = 0; i < row; ++i)
            for(int j = 0; j < m2.column; ++j)
                for(int k = 0; k < column; ++k)
                {
                    T v=( matrix[i*column+k] * m2.matrix[j+k*m2.column]);
                    mul[i*m2.column+j]=mul[i*m2.column+j]+v;
                }

        m2.matrix=mul;
        m2.row=row;
        return m2;
    }

    void transpose()
    {
        lp(i,column)
        {
            lp(j,row)cout << matrix[i+j*column]<< (j!=row-1?" ":"");
            if(i!=column-1) cout << endl;
        }
    }


};






template<typename T>
ostream & operator << (ostream &out, const Matrix<T> &m)
{
    lp(i,m.row)
    {
        lp(j,m.column)
        {

            out << m.matrix[i*m.column+j];
            if(j!=m.column-1)out << " ";

        }

        if(i!=m.row-1)out<<endl;
    }

    return out;
}
template<typename T>
istream & operator >> (istream &in,  Matrix<T> &m)
{
    lp(i,m.row)
    lp(j,m.column)
    in >> m.matrix[i*m.column+j];

    return in;
}


int main(){
    int a , b;
    cin >> a >> b;
    Matrix<double> A(a,b),B(a,b);
    cin >> A >>a>>b>> B ;
    Matrix<double>sum(a,b);
    sum = A+B;
    cout <<fixed << setprecision(2)<< sum << endl;
    cin >> a >> b;

    Matrix<Fraction> X(a,b),Y(a,b),mul(a,b);
    cin >> X >>a>>b>> Y;
    X.initial=0;
    mul=X*Y;
    cout << mul;

}
