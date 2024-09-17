//! # Geo-AID's optimization engines.
//!
//! Details on how each works is provided in the math doc,
//! but generic, simplified descriptions are also provided here.

use std::collections::VecDeque;

pub mod compiler;
pub mod glide;
pub mod rage;
mod thread_pool;

/// Helper struct for managing mean quality of last `n` qualities.
///
/// Detects if the arithmetic mean of last `entry_count` quality deltas exceeds `limit`.
pub(crate) struct QualityRecord {
    queue: VecDeque<f64>,
    entry_count: usize,
    sum: f64,
    limit: f64,
    quality: f64,
}

impl QualityRecord {
    /// Create a new quality record that counts the last `entry_count` qualities
    /// And reports that generation should stop when their arithmetic mean exceeds `limit`.
    #[must_use]
    pub fn new(entry_count: usize, limit: f64) -> Self {
        #[allow(clippy::cast_precision_loss)]
        Self {
            queue: VecDeque::with_capacity(entry_count),
            entry_count,
            sum: 0.0,
            limit: limit * entry_count as f64,
            quality: 0.0,
        }
    }

    /// Returns whether after pushing the quality generation should be stopped.
    ///
    /// DOES NOT guard against providing a lower quality than the current best.
    pub fn record(&mut self, quality: f64) -> bool {
        if self.queue.len() >= self.entry_count {
            // Last quality needs to be popped.
            let q = self.queue.pop_front().unwrap_or_default();
            self.sum -= q;
        }

        let delta = quality - self.quality;
        self.sum += delta;
        self.queue.push_back(delta);
        self.quality = quality;

        self.queue.len() >= self.entry_count && self.sum <= self.limit
    }

    /// Clears the quality record, therefore resetting the process.
    pub fn clear(&mut self) {
        self.queue.clear();
        self.sum = 0.0;
        self.quality = 0.0;
    }

    /// Get current (latest) quality
    pub fn get_quality(&self) -> f64 {
        self.quality
    }
}
