//! Implements `ThreadExecutor`.

use std::time::Duration;
use std::cell::RefCell;
use std::sync::Arc;
use std::sync::mpsc::{channel, Sender, Receiver, TryRecvError};
use uci::{SetOption, OptionDescription};
use search::*;


/// Turns a `SearchThread` into `SearchExecutor`.
pub struct ThreadExecutor<T: Search> {
    tt: Arc<T::HashTable>,
    messages_tx: Sender<String>,
    reports_rx: Receiver<SearchReport<T::ReportData>>,
    reports_tx: Sender<SearchReport<T::ReportData>>,
    pending_report: RefCell<Option<SearchReport<T::ReportData>>>,
}


impl<T: Search> SearchExecutor for ThreadExecutor<T> {
    type HashTable = T::HashTable;

    type SearchNode = T::SearchNode;

    type ReportData = T::ReportData;

    fn new(tt: Arc<Self::HashTable>) -> Self {
        let (reports_tx, reports_rx) = channel();
        Self {
            tt: tt,
            messages_tx: channel().0,
            reports_rx: reports_rx,
            reports_tx: reports_tx,
            pending_report: RefCell::new(None),
        }
    }

    fn start_search(&mut self, params: SearchParams<Self::SearchNode>) {
        let (messages_tx, messages_rx) = channel();
        self.messages_tx = messages_tx;
        T::start_thread(params,
                        self.tt.clone(),
                        self.reports_tx.clone(),
                        messages_rx);
    }

    fn wait_report(&self, timeout_after: Duration) {
        let mut report = self.pending_report.borrow_mut();
        if report.is_none() {
            *report = self.reports_rx.recv_timeout(timeout_after).ok();
        }
    }

    fn try_recv_report(&mut self) -> Result<SearchReport<Self::ReportData>, TryRecvError> {
        self.pending_report
            .borrow_mut()
            .take()
            .ok_or(TryRecvError::Empty)
            .or_else(|_| self.reports_rx.try_recv())
    }

    fn send_message(&mut self, msg: &str) {
        self.messages_tx.send(msg.to_string()).ok();
    }
}


impl<T: Search> SetOption for ThreadExecutor<T> {
    fn options() -> Vec<(String, OptionDescription)> {
        T::options()
    }

    fn set_option(name: &str, value: &str) {
        T::set_option(name, value);
    }
}
