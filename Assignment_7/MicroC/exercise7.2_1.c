void arrsum(int n, int arr[], int *sump) {
    *sump = 0;  
    int i;
    i = 0;
    while(i < n) {
        *sump = *sump + arr[i];
        i = i +1;  
    }
}

int main(int n) {
    int arr[4];  
    arr[0] = 7;  
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;
    
    int sum;
    arrsum(n, arr, &sum);  
    print sum;
}
