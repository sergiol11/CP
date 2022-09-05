#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include "options.h"

#define MAX_AMOUNT 20

struct bank {
    int num_accounts;        // number of accounts
    int *accounts;           // balance array  (valor de cada conta)
    pthread_mutex_t *mutexs;  //array de mutex (un por conta)
};

//Argumentos do thread
struct args {
    int          thread_num;  // application defined thread #
    int          delay;       // delay between operations
    //int	         iterations;  // number of operations
    int          net_total;   // total amount deposited by this thread  o total que depositou o thread
    struct bank *bank;        // pointer to the bank (shared with other threads)
    //Contadores para cada operación con respectivos mutex
    int *iterations_depositos;
    int *iterations_transferencias;
    pthread_mutex_t *mutex_depositos;
    pthread_mutex_t *mutex_transferencias;
    //Bool para execución do thread printer con respectivo mutex
    bool *printer;
    pthread_mutex_t *mutex_printer;
};

struct thread_info {
    pthread_t    id;    // id returned by pthread_create()
    struct args *args;  // pointer to the arguments
};

//Función auxiliar para execución dos depósitos e transferencias
bool continuation(int *contador, pthread_mutex_t *mutex){
    bool seguir = false;

    pthread_mutex_lock(mutex);
    if (--*contador >= 0) {  //Mentres non fagamos todas as iteracións devolvemos true para seguir realizando a operación
        seguir = true;
    }
    pthread_mutex_unlock(mutex);

    return seguir;
}

// Threads run on this function
void *deposit(void *ptr){
    struct args *args =  ptr;
    int amount, account, balance;


    while(continuation(args->iterations_depositos, args->mutex_depositos)) {  //COMPROBAMOS SE SEGUIMOS FACENDO DEPÓSITOS
        amount  = rand() % MAX_AMOUNT;
        account = rand() % args->bank->num_accounts;

        pthread_mutex_lock(&args->bank->mutexs[account]);

        printf("Thread %d depositing %d on account %d\n",
            args->thread_num, amount, account);

        balance = args->bank->accounts[account];
        if(args->delay) usleep(args->delay); // Force a context switch

        balance += amount;
        if(args->delay) usleep(args->delay);

        args->bank->accounts[account] = balance;
        if(args->delay) usleep(args->delay);

        args->net_total += amount;
        pthread_mutex_unlock(&args->bank->mutexs[account]);
    }
    return NULL;
}


//Función transferencia
void *transfer(void *ptr){
    struct args *args = ptr;
    int amount, account1, account2, balance_c1;

    //-- 1º Comprobamos que teñamos 2 contas, se solo hai unha non se pode facer transferencia
    if(args->bank->num_accounts <= 1){
        printf("Transference not posible\n");
        return NULL;
    }

    while(continuation(args->iterations_transferencias, args->mutex_transferencias)){  //COMPROBAMOS SE SEGUIMOS FACENDO TRANSFERENCIAS
        //-- Obtemos contas
        account1 = rand() % args->bank->num_accounts;
        do{
            account2 = rand() % args->bank->num_accounts;    //Vemos que a 2ª non sexa a misma que a 1ª!
        }while(account2 == account1);

        //-- Forma de evitar interbloqueo! Reserva Ordenada!
        if(account1 < account2){
            pthread_mutex_lock(&args->bank->mutexs[account1]);
            pthread_mutex_lock(&args->bank->mutexs[account2]);
        }else{
            pthread_mutex_lock(&args->bank->mutexs[account2]);
            pthread_mutex_lock(&args->bank->mutexs[account1]);
        }

        //-- Obtemos a cantidade a transferir (amount)
        balance_c1 = args->bank->accounts[account1];  //Diñeiro inicial cuenta1
        if(args->delay) usleep(args->delay); // Force a context switch
        if(balance_c1 != 0){
            amount = rand() % balance_c1;
        }else{
            amount = 0;
        }


        printf("Thread %d transfers %d from account %d to account %d\n",
               args->thread_num, amount, account1, account2);


        //-- Facemos operacións de transferencia
        balance_c1 -= amount;
        if(args->delay) usleep(args->delay); // Force a context switch

        args->bank->accounts[account2] += amount;
        if(args->delay) usleep(args->delay); // Force a context switch

        args->bank->accounts[account1] = balance_c1;
        if(args->delay) usleep(args->delay); // Force a context switch

        //Liberamos mutex
        pthread_mutex_unlock(&args->bank->mutexs[account2]);
        pthread_mutex_unlock(&args->bank->mutexs[account1]);
    }
    return NULL;
}


void *print_counts(void *ptr){
    struct args *args =  ptr;
    int i, saldo = 0;  //Saldo indica a suma de todas as contas

    //-- Bloqueamos o mutex a primeira vez que usamos a variable (así non se modifica valor)
    pthread_mutex_lock(args->mutex_printer);
    while(*args->printer) {
        pthread_mutex_unlock(args->mutex_printer);   //Se entramos aquí desbloqueamos o mutex de printer

        //-- Establecemos espera:
        if (args->delay) usleep(args->delay); // Force a context switch

        //-- Blqueamos os mutex de todas as contas
        for (i = 0; i < args->bank->num_accounts; i++) {
            pthread_mutex_lock(&args->bank->mutexs[i]);
        }

        //-- Escribimos a situación das contas:
        printf("\n\n**********Accounts Balance***********\n");
        for (i = 0; i < args->bank->num_accounts; i++) {
            printf("%d: %d\n", i, args->bank->accounts[i]);
            saldo += args->bank->accounts[i];
        }
        printf("Total: %d\n", saldo);
        printf("\n\n");

        //-- Establecemos espera:
        if (args->delay) usleep(args->delay); // Force a context switch

        //-- Liberamos mutexs
        for (i = 0; i < args->bank->num_accounts; i++) {
            pthread_mutex_unlock(&args->bank->mutexs[i]);
        }

        //-- Volvemos poñer o saldo a 0 (senón en seguinte iteración do while sigue sumando)
        saldo = 0;

        //-- Bloqueamos o mutex para cando volvamos a ver a condición do while
        pthread_mutex_lock(args->mutex_printer);
    }
    pthread_mutex_unlock(args->mutex_printer);
    return NULL;
}


// start opt.num_threads threads running on deposit.   INICIALIZAMOS THREADS
struct thread_info *start_threads(struct options opt, struct bank *bank){
    int i;
    struct thread_info *threads;
    int *contador_depositos, *contador_transferencias;
    pthread_mutex_t *mutex_depositos, *mutex_transferencias;
    bool *printer;
    pthread_mutex_t *mutex_printer;

    //-- ASIGNAMOS MEMORIA AOS CONTADORES E INICIALIZAMOLOS
    contador_depositos = malloc(sizeof(int));
    *contador_depositos = opt.iterations;
    contador_transferencias = malloc(sizeof(int));
    *contador_transferencias = opt.iterations;

    //-- ASIGNAMOS MEMORIA AOS MUTEX DOS CONTADORES E INICIALIZAMOLOS
    mutex_depositos = malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex_depositos, NULL);
    mutex_transferencias = malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex_transferencias, NULL);


    //--ASIGNAMOS MEMORIA E INICIALIZAMOS BOOL PARA O PRINTER E O SEU MUTEX
    printer = malloc(sizeof(bool));
    *printer = true;  //Empezamos a true e SOLO poñemos a false se acabaron todos os threads que fan transfer
    mutex_printer = malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex_printer, NULL);


    //-- RESERVAMOS MEMORIA NECESARIA PARA TODOS OS THREADS
    threads = malloc(sizeof(struct thread_info) * (opt.num_threads * 2) + sizeof(struct thread_info));


    if (threads == NULL) {
        printf("Not enough memory\n");
        exit(1);
    }


    //-- CREAMOS OS THREADS QUE FAN DEPÓSITOS
    printf("creating %d threads  (deposit)\n", opt.num_threads);

    // Create num_thread threads running swap()
    for (i = 0; i < opt.num_threads; i++) {
        threads[i].args = malloc(sizeof(struct args));

        threads[i].args -> thread_num = i;
        threads[i].args -> net_total  = 0;
        threads[i].args -> bank       = bank;
        threads[i].args -> delay      = opt.delay;
        threads[i].args->iterations_depositos = contador_depositos;
        threads[i].args->iterations_transferencias = contador_transferencias;
        threads[i].args->mutex_depositos = mutex_depositos;
        threads[i].args->mutex_transferencias = mutex_transferencias;


        if (0 != pthread_create(&threads[i].id, NULL, deposit, threads[i].args)) {
            printf("Could not create thread #%d", i);
            exit(1);
        }
    }

    //-- ESPERAMOS  QUE ACABEN THREADS QUE FAN deposit PARA CREAR OS QUE FAN TRANSFERENCIAS E O QUE ESCRIBE SITUACIÓN DAS CONTAS
    for (i = 0; i < opt.num_threads; i++)
        pthread_join(threads[i].id, NULL);



    //-- CREAMOS THREAD QUE ESCRIBE SITUACIÓN DAS CONTAS PERIODICAMENTE
    printf("\ncreating thread  (printer)\n");
    threads[opt.num_threads].args = malloc(sizeof(struct args));

    threads[opt.num_threads].args -> thread_num = opt.num_threads;
    threads[opt.num_threads].args -> net_total  = 0;
    threads[opt.num_threads].args -> bank       = bank;
    threads[opt.num_threads].args -> delay      = opt.delay;
    threads[opt.num_threads].args -> printer = printer;
    threads[opt.num_threads].args -> mutex_printer = mutex_printer;

    if (0 != pthread_create(&threads[opt.num_threads].id, NULL, print_counts, threads[opt.num_threads].args)) {
        printf("Could not create thread #%d", opt.num_threads);
        exit(1);
    }



    //-- CREAMOS THREADS PARA TRANSFERENCIAS
    printf("\ncreating %d threads  (transfer)\n", opt.num_threads);
    for (i = 0; i < opt.num_threads; i++) {
        threads[i + opt.num_threads + 1].args = malloc(sizeof(struct args));

        threads[i + opt.num_threads + 1].args -> thread_num = i + opt.num_threads + 1;  //O + 1 é por haber inicializado o thread que di situacion antes!!!!
        threads[i + opt.num_threads + 1].args -> net_total  = 0;
        threads[i + opt.num_threads + 1].args -> bank       = bank;
        threads[i + opt.num_threads + 1].args -> delay      = opt.delay;
        //threads[i + opt.num_threads + 1].args -> iterations = opt.iterations;
        threads[i + opt.num_threads + 1].args->iterations_depositos = contador_depositos;
        threads[i + opt.num_threads + 1].args->iterations_transferencias = contador_transferencias;
        threads[i + opt.num_threads + 1].args->mutex_depositos = mutex_depositos;
        threads[i + opt.num_threads + 1].args->mutex_transferencias = mutex_transferencias;


        if (0 != pthread_create(&threads[i + opt.num_threads + 1].id, NULL, transfer, threads[i + opt.num_threads + 1].args)) {
            printf("Could not create thread #%d", i + opt.num_threads + 1);
            exit(1);
        }
    }

    return threads;
}



// Print the final balances of accounts and threads
void print_balances(struct bank *bank, struct thread_info *thrs, int num_threads) {
    int total_deposits=0, bank_total=0;
    printf("\nNet deposits by thread\n");

    for(int i=0; i < num_threads; i++) {
        printf("%d: %d\n", i, thrs[i].args->net_total);
        total_deposits += thrs[i].args->net_total;
    }
    printf("Total: %d\n", total_deposits);

    printf("\nAccount balance\n");
    for(int i=0; i < bank->num_accounts; i++) {
        printf("%d: %d\n", i, bank->accounts[i]);
        bank_total += bank->accounts[i];
    }
    printf("Total: %d\n", bank_total);
}

// wait for all threads to finish, print totals, and free memory
void wait(struct options opt, struct bank *bank, struct thread_info *threads) {
    //-- Esperamos polos threads que fan transferencias a que acaben
    for (int i = opt.num_threads + 1; i < opt.num_threads*2 + 1; i++) {
        pthread_join(threads[i].id, NULL);
    }

    //-- Unha vez acabaron os que fan transfer poñemos o bool de printer a false
    pthread_mutex_lock(threads[opt.num_threads].args->mutex_printer);
    *threads[opt.num_threads].args->printer = false;
    pthread_mutex_unlock(threads[opt.num_threads].args->mutex_printer);

    //-- Esperamos a que acabe o thread printer
    pthread_join(threads[opt.num_threads].id, NULL);

    //-- Liberamos memoria de args extra
    free(threads[opt.num_threads].args->mutex_printer);
    free(threads[opt.num_threads].args->printer);
    free(threads[opt.num_threads + 1].args->iterations_depositos);
    free(threads[opt.num_threads + 1].args->iterations_transferencias);
    free(threads[opt.num_threads + 1].args->mutex_depositos);
    free(threads[opt.num_threads + 1].args->mutex_transferencias);


    print_balances(bank, threads, opt.num_threads);

    //-- Liberamos memoria restante
    for (int i = 0; i < opt.num_threads*2 + 1; i++)
        free(threads[i].args);

    free(threads);
    free(bank->accounts);
    free(bank->mutexs);
}

// allocate memory, and set all accounts to 0
void init_accounts(struct bank *bank, int num_accounts) {
    //-- Inicializamos as contas e os seus respectivos mutex
    bank->num_accounts = num_accounts;
    bank->accounts     = malloc(bank->num_accounts * sizeof(int));
    bank->mutexs = malloc(bank->num_accounts * sizeof(pthread_mutex_t)); //ASIGNAMOS MEMORIA AO ARRAY DE MUTEX!!!

    for(int i=0; i < bank->num_accounts; i++) {
        bank->accounts[i] = 0;

        pthread_mutex_init(&bank->mutexs[i],NULL);
    }
}


int main (int argc, char **argv){
    struct options      opt;
    struct bank         bank;
    struct thread_info *thrs;

    srand(time(NULL));

    // Default values for the options
    opt.num_threads  = 5;
    opt.num_accounts = 10;
    opt.iterations   = 100;  //Nº iteracións de cada operación
    opt.delay        = 10;

    read_options(argc, argv, &opt);

    init_accounts(&bank, opt.num_accounts);

    thrs = start_threads(opt, &bank);
    wait(opt, &bank, thrs);
    return 0;
}
