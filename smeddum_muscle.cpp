#include <cmath>

extern "C" {
    double calculate_resonance(int active_pixels, int total_pixels) {
        if (total_pixels <= 0) return 0.0;
        double density = (double)active_pixels / (double)total_pixels;
        return 171.5 * std::sqrt(density);
    }
}
