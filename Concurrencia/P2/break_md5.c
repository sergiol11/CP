#include <sys/types.h>
#include <openssl/md5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>

#define PASS_LEN 6
#define DIFERENCIA 1000
#define THREADS_BREAK 12


struct info_hashes{
    unsigned char **hashes_md5;    //hashes que vamos a descifrar (md5)
    char **hashes_original;     //hashes formato "string"
    int num_hashes;    //Número de hashes a descifrar
    bool *comprobar_hash;   //Array de bool para cada hash (true -> temos que descifrar, false -> non temos que descifrar)
    pthread_mutex_t *mutex_bool_hashes;    //Mutex de protección de acceso a calquera dos bool para comprobar hash
};

struct args{
    struct info_hashes *info_hashes;
   int *num_pass_probadas;   //Número de contraseñas probadas
   pthread_mutex_t *mutex_num_probadas;    //Mutex para num_pass_probadas
   bool *encontrados;       //Bool que indica se encontramos os hashes que buscamos (true -> encontrámolos todos, false -> aínda non)
   pthread_mutex_t *mutex_encontrados;    //Mutex para a variable encontrados
};


struct thread_info{
    pthread_t id;
    struct args *args;
};



long ipow(long base, int exp)
{
    long res = 1;
    for (;;)
    {
        if (exp & 1)
            res *= base;
        exp >>= 1;
        if (!exp)
            break;
        base *= base;
    }

    return res;
}

long pass_to_long(char *str) {
    long res = 0;

    for(int i=0; i < PASS_LEN; i++)
        res = res * 26 + str[i]-'a';

    return res;
};

void long_to_pass(long n, unsigned char *str) {  // str should have size PASS_SIZE+1
    for(int i=PASS_LEN-1; i >= 0; i--) {
        str[i] = n % 26 + 'a';
        n /= 26;
    }
    str[PASS_LEN] = '\0';
}

int hex_value(char c) {
    if (c>='0' && c <='9')
        return c - '0';
    else if (c>= 'A' && c <='F')
        return c-'A'+10;
    else if (c>= 'a' && c <='f')
        return c-'a'+10;
    else return 0;
}

void hex_to_num(char *str, unsigned char *hex) {
    for(int i=0; i < MD5_DIGEST_LENGTH; i++)
        hex[i] = (hex_value(str[i*2]) << 4) + hex_value(str[i*2 + 1]);
}

//Vemos se xa encontramos todos os hashes
void search_finished(struct args *args){
     bool todos = true;

    pthread_mutex_lock(args->info_hashes->mutex_bool_hashes);
     for(int i = 0; i < args->info_hashes->num_hashes; i++){
         if(args->info_hashes->comprobar_hash[i]){
            todos = false;
            break;
         }
     }
    pthread_mutex_unlock(args->info_hashes->mutex_bool_hashes);

     if(todos){
         pthread_mutex_lock(args->mutex_encontrados);
         *args->encontrados = true;
         pthread_mutex_unlock(args->mutex_encontrados);
     }

}


void *break_pass(void *ptr) {
    int i, j;
    struct args *args = ptr;
    unsigned char res[MD5_DIGEST_LENGTH];
    unsigned char *pass = malloc((PASS_LEN + 1) * sizeof(char));
    long bound = ipow(26, PASS_LEN); // we have passwords of PASS_LEN
                                     // lowercase chars =>
                                    //     26 ^ PASS_LEN  different cases
    int aux;
    bool *copia;
    copia = malloc(sizeof(bool) * args->info_hashes->num_hashes);

    //Usamos unha copia dos bool de hashes que xa foron comprobados e dos que non para aforrar facer lock e unlock
    for(i = 0; i < args->info_hashes->num_hashes; i++){
        copia[i] = args->info_hashes->comprobar_hash;
    }


    while(1) {   //-- Bucle no que se busca contraseña
        pthread_mutex_lock(args->mutex_num_probadas);
        pthread_mutex_lock(args->mutex_encontrados);

        //-- SE XA ENCONTRAMOS OU ACABAMOS
        if(*args->num_pass_probadas >= bound || *args->encontrados){
            pthread_mutex_unlock(args->mutex_num_probadas);
            pthread_mutex_unlock(args->mutex_encontrados);
            free(pass);
            free(copia);
            return NULL;
        }

        //-- SE NON ACABAMOS indicamos espacio onde empeza a comprobar hashes novo thread:
        pthread_mutex_unlock(args->mutex_encontrados);

        aux = *args->num_pass_probadas;  //Inicio

        *args->num_pass_probadas += DIFERENCIA; //Fin

        pthread_mutex_unlock(args->mutex_num_probadas);

        //-- Neste bucle comprobamos os hashes correspondientes
        for(i = aux; i < aux + DIFERENCIA; i++) {
            long_to_pass(i, pass);

            MD5(pass, PASS_LEN, res);

            //-- Cada hash comparámolo con todos os que temos que buscar que aínda non foran encontrados
            for(j = 0; j < args->info_hashes->num_hashes; j++) {

                if(copia[j]) {  //-- Vemos se temos que comparar con ese hash ou no (se xa o desciframos)

                    //ENCONTRAMOS A CONTRASEÑA
                    if (0 == memcmp(res, args->info_hashes->hashes_md5[j], MD5_DIGEST_LENGTH)) {
                        pthread_mutex_lock(args->info_hashes->mutex_bool_hashes);
                        args->info_hashes->comprobar_hash[j] = false;   //Actualizamos bool para non seguir buscando
                        copia[j] = false;  //Actualizamos copia tamén
                        pthread_mutex_unlock(args->info_hashes->mutex_bool_hashes);

                        //-- Vemos se xa encontramos todos os hashes ou non
                        search_finished(args);

                        //Escribimos mensaje de que xa encontramos o hash correspondente
                        printf("\n%s: %s\n", args->info_hashes->hashes_original[j], pass);


                        //1ª Opción, xa encontramos todas as contraseñas
                        if(*args->encontrados){
                            free(pass);
                            free(copia);
                            return NULL; // Found it!   SALE DO BUCLE FOR INMEDIATAMENTE!
                        }else{  //2ª Opción, aínda non encontramos todas as contraseñas
                        break;
                        }
                    }

                }
            }
        }

    }
}

double what_time_is_it(){
    struct timespec now;
    clock_gettime(CLOCK_REALTIME, &now);
    return now.tv_sec + now.tv_nsec*1e-9;
}

//Barra de progreso
void *loader(void *ptr){
    struct args *args = ptr;
    long contrasenas = ipow(26, PASS_LEN);  //Nº total de hashes
    char *buffer = malloc(101);   //gardo memoria para buffer
    long porcentaje;
    long contador_probadas;
    long pass_probadas_anterior;
    double velocidad;
    double t1, t2, diff;


    buffer[100] = '\0';

    //-- Gardamos o valor das contraseñas contadas ata o momento
    pthread_mutex_lock(args->mutex_num_probadas);
    pass_probadas_anterior = (long)*args->num_pass_probadas;

    pthread_mutex_unlock(args->mutex_num_probadas);

    //Empezo a contar
    t1 = what_time_is_it();

    usleep(10);  //Retraso para non ocupar a CPU demasiado

    //Paro de contar
    pthread_mutex_lock(args->mutex_num_probadas);
    t2 = what_time_is_it();

    //Calculo velocidad
    contador_probadas = (long)*args->num_pass_probadas - pass_probadas_anterior;
    diff = t2 - t1;
    velocidad = (double)contador_probadas/diff;  //Velocidad en segundos!

    //Cálculo do porcentaje
    porcentaje = ((long)*args->num_pass_probadas * 100) / contrasenas;

    pthread_mutex_lock(args->mutex_encontrados);
    while(porcentaje < 100 && !*args->encontrados){    //Bucle para escribir barra!


        for (int i = 0; i < 99; i++){
            if (i < porcentaje)
                buffer[i] = '*';
            else buffer[i] = ' ';
        }


        //Escribimos situación
        printf("[%s]  %ld%%  %.0f pswd/s\r", buffer, porcentaje, velocidad);
        fflush(stdout);  //Limpiamos o buffer


        //CALCULO DE TEMPO
        //0 Antes de desbloquear os mutex gardo as contraseñas probadas anteriormente
        pass_probadas_anterior = (long)*args->num_pass_probadas;

        //1º Desbloqueo ambos mutex e póñome a contar tempo
        pthread_mutex_unlock(args->mutex_encontrados);
        pthread_mutex_unlock(args->mutex_num_probadas);

        //2º Empezo a contar
        t1 = what_time_is_it();

        usleep(1000);  //Retraso para non ocupar a CPU demasiado

        //3º Paro de contar
        pthread_mutex_lock(args->mutex_num_probadas);
        t2 = what_time_is_it();    //Acabamos de contar

        //Calculo velocidad
        contador_probadas = (long)*args->num_pass_probadas - pass_probadas_anterior;
        diff = t2 - t1;
        velocidad = (double)contador_probadas/diff;  //Velocidad en segundos!


        //Novo porcentaje
        porcentaje = ((long)*args->num_pass_probadas * 100) / contrasenas;

        pthread_mutex_lock(args->mutex_encontrados);
    }
    //Desbloqueamos ambos mutex por se non entramos no bucle
    pthread_mutex_unlock(args->mutex_encontrados);
    pthread_mutex_unlock(args->mutex_num_probadas);

    //Avisamos aos threads que buscan contraseñas que paren se chegamos ao 100%
    if(porcentaje >= 100){
        pthread_mutex_lock(args->mutex_encontrados);
        *args->encontrados = true;
        pthread_mutex_unlock(args->mutex_encontrados);

        for(int i = 0; i < args->info_hashes->num_hashes; i++){  //Advertencia de que hash está fóra de rango!
            if(args->info_hashes->comprobar_hash[i] == true){
                printf("\n%s: fóra de rango!\n", args->info_hashes->hashes_original[i]);
                break;
            }
        }
    }

    free(buffer);
    return NULL;
}



struct thread_info *start_threads(struct info_hashes *info){
   struct thread_info *threads;
   int *num_pass_probadas, i;
   bool *encontrados;
   pthread_mutex_t *mutex_num_probadas, *mutex_encontrados;

   //-- Reservamos memoria para os threads
   threads = malloc(sizeof(struct thread_info)* (THREADS_BREAK + 1));

    if (threads == NULL) {
        printf("Not enough memory\n");
        exit(1);
    }


    //-- Inicializamos o contador de contraseñas probadas e o seu mutex
    num_pass_probadas = malloc(sizeof(int));
    *num_pass_probadas = 0;
    mutex_num_probadas = malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex_num_probadas, NULL);

    //-- Inicializamos o bool para decir cando se atoparon todas as contraseñas
    encontrados = malloc(sizeof(bool));
    *encontrados = false;  //Inicialmente a false porque non a encontramos
    mutex_encontrados = malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex_encontrados, NULL);



    //-- CREAMOS THREAD PARA ESCRIBIR BARRA
    threads[0].args = malloc(sizeof(struct args));

    threads[0].args->info_hashes = info;
    threads[0].args->  num_pass_probadas = num_pass_probadas;
    threads[0].args->  mutex_num_probadas = mutex_num_probadas;
    threads[0].args->  encontrados = encontrados;
    threads[0].args->  mutex_encontrados = mutex_encontrados;

    if (0 != pthread_create(&threads[0].id, NULL, loader, threads[0].args)) {
        printf("Could not create thread #%d", 0);
        exit(1);
    }


    //-- CREAMOS THREADS PARA ENCONTRAR A CONTRASEÑA
    for(i = 1; i < THREADS_BREAK + 1; i++) {
        threads[i].args = malloc(sizeof(struct args));

        threads[i].args->info_hashes = info;
        threads[i].args->  num_pass_probadas = num_pass_probadas;
        threads[i].args->  mutex_num_probadas = mutex_num_probadas;
        threads[i].args->  encontrados = encontrados;
        threads[i].args->  mutex_encontrados = mutex_encontrados;

        if (0 != pthread_create(&threads[i].id, NULL, break_pass, threads[i].args)) {
            printf("Could not create thread #%d", 0);
            exit(1);
        }
    }

    return threads;
}


void wait(struct thread_info *threads, struct info_hashes *info){
    int i;
    //Esperamos que acaben os threads que buscan contraseña
    for(i = 1; i < THREADS_BREAK + 1; i++) {
        pthread_join(threads[i].id, NULL);
    }

    //Esperamos que acabe o thread loader (barra)
    pthread_join(threads[0].id, NULL);

    //-- CANDO ACABEN OS THREADS LIBERAMOS MEMORIA
    free(threads[0].args->num_pass_probadas);
    free(threads[0].args->mutex_num_probadas);
    free(threads[0].args->encontrados);
    free(threads[0].args->mutex_encontrados);


    //ELiminamos memoria thread a thread
    for(i = 0; i < THREADS_BREAK + 1; i++) {
        free(threads[i].args);
    }

    free(threads);

    //Eliminamos memoria de info
    free(info->comprobar_hash);
    free(info->mutex_bool_hashes);
    for(i = 0; i < info->num_hashes; i++){
        free(info->hashes_md5[i]);
    }
    free(info->hashes_md5);
}


//-- Inicializamos info dos hashes
void init_hashes(struct info_hashes *info, char *hashes[], int num_hashes){
    int i;
    unsigned char *md5_num;

    //Pasamos cada hash de string a md5
    info->hashes_md5 = malloc(sizeof(unsigned char *) * num_hashes);
    for(i = 0; i < num_hashes; i++){
        md5_num = malloc(MD5_DIGEST_LENGTH);
        hex_to_num(hashes[i], md5_num);
        info->hashes_md5[i] = md5_num;
    }

    //En string
    info->hashes_original = hashes;

    info->num_hashes = num_hashes;
    info->comprobar_hash = malloc(sizeof(bool) * num_hashes);
    info->mutex_bool_hashes = malloc(sizeof(pthread_mutex_t));

    pthread_mutex_init(info->mutex_bool_hashes, NULL);

    for(i = 0; i < num_hashes; i++){
        info->comprobar_hash[i] = true;
    }

}



int obtener_num_hashes(char *argv[]){
    int hashes = 0, i = 1;

    while(argv[i] != NULL){
        hashes++;
        i++;
    }

    return hashes;
}


int main(int argc, char *argv[]) {
    struct thread_info *threads;
    struct info_hashes info;
    int i;
    char *hashes[obtener_num_hashes(argv)];

    if(argc < 2) {
        printf("Use: %s string\n", argv[0]);
        exit(0);
    }


    for(i = 1; i < obtener_num_hashes(argv)+1; i++){
    hashes[i-1] = argv[i];
    }

    init_hashes(&info, hashes, obtener_num_hashes(argv));

    threads = start_threads(&info);
    wait(threads, &info);
    return 0;
}
