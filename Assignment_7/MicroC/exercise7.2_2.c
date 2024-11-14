void main(int n) {
    int arr[20];
    arr[0] = 7;
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;
    arr[4] = 7;
    arr[5] = 13;
    arr[6] = 9;
    arr[7] = 8;
    arr[8] = 7;
    arr[9] = 13;
    arr[10] = 9;
    arr[11] = 8;
    arr[12] = 7;
    arr[13] = 13;
    arr[14] = 9;
    arr[15] = 8;
    arr[16] = 7;
    arr[17] = 13;
    arr[18] = 9;
    arr[19] = 8;

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