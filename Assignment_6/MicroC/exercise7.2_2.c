void main(int n) {
    int arr[20];
    arr[0] = 7;
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;

    square(n, arr);

    int sump;
    arrsum(n, arr, &sump);

    print sump;
}

void arrsum(int n, int arr[], int *sump) {
    *sump = 0;  
    int i;
    i = 0;
    while(i < n) {
        *sump = *sump + arr[i];
        i = i +1;  
    }
}

void square(int n, int arr[]){
    int i;
    i = 0;

    while(i < n){
        arr[i] = arr[i] * arr[i];
        i = i+1;
    }
}