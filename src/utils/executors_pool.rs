use std::time::Duration;
use std::sync::Arc;
use std::thread::{spawn, sleep};
use std::sync::mpsc::{channel, Sender, Receiver, TryRecvError};
use search_executor::{SearchParams, SearchReport, SearchExecutor};
use hash_table::*;


pub struct ExecutorsPool<T: SearchExecutor> {
    reports: Receiver<SearchReport<T::ReportData>>,
    message_queues: Vec<Sender<String>>,
}


impl<T: SearchExecutor> ExecutorsPool<T> {
    fn new(pool_size: usize, tt: Arc<T::HashTable>) -> Self {
        let (reports_tx, reports_rx) = channel();
        let mut message_queues: Vec<Sender<String>> = vec![];
        for _ in 0..pool_size {
            let tt = tt.clone();
            let reports_tx = reports_tx.clone();
            let (messages_tx, messages_rx) = channel();
            message_queues.push(messages_tx);
            let read_thread = spawn(move || {
                let mut search_executor = T::new(tt);
                loop {
                    if let Ok(msg) = messages_rx.try_recv() {
                        search_executor.send_message(&msg);
                    }
                    search_executor.wait_report(Duration::from_millis(25));
                    if let Ok(report) = search_executor.try_recv_report() {
                        reports_tx.send(report).unwrap();
                    }
                }
            });
        }

        ExecutorsPool {
            reports: reports_rx,
            message_queues: message_queues,
        }
    }
}
