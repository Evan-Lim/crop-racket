#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

static float mock_moisture = 50.0;
static float mock_temp = 25.0;
static bool mock_button = false;
static bool mock_flow = false;

__attribute__((weak)) float sensor_moisture_read_opt(void) { return mock_moisture; }
__attribute__((weak)) float sensor_temp_read_opt(void) { return mock_temp; }
__attribute__((weak)) bool sensor_button_read_opt(void) { return mock_button; }
__attribute__((weak)) bool sensor_flow_meter_read_opt(void) { return mock_flow; }

static bool last_led = false;
static bool last_water_pump = false;
static bool last_heater = false;

__attribute__((weak)) void digitalWrite(int pin, int val) {
    bool state = (val == 1);
    if (pin == 4 && state != last_led) {
        printf("[OUTPUT] led -> %s\n", state ? "ON" : "OFF"); last_led = state;
    }
    if (pin == 2 && state != last_water_pump) {
        printf("[OUTPUT] water_pump -> %s\n", state ? "ON" : "OFF"); last_water_pump = state;
    }
    if (pin == 3 && state != last_heater) {
        printf("[OUTPUT] heater -> %s\n", state ? "ON" : "OFF"); last_heater = state;
    }
}

__attribute__((weak)) int analogRead(int pin) { return 512; }

static unsigned long sim_millis = 0;
__attribute__((weak)) unsigned long millis(void) { return sim_millis; }

int main(void) {
    extern void setup(void) __attribute__((weak));
    extern void loop(void) __attribute__((weak));
    if (setup) setup();

    printf("============================================================\n");
    printf("  CROP Simulation Environment (Racket)\n");
    printf("============================================================\n");
    printf("Commands: button <0|1> | moisture <f> | temp <f> | s | q\n\n");

    char line[256];
    while (1) {
        if (loop) loop();
        sim_millis += 10;
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) break;
        line[strcspn(line, "\n")] = '\0';
        if (strlen(line) == 0) continue;
        char cmd[32]; float val;
        if (sscanf(line, "%31s %f", cmd, &val) == 2) {
            if (!strcmp(cmd, "button")) { mock_button = (val != 0); printf("Button %s\n", mock_button ? "ON" : "OFF"); }
            else if (!strcmp(cmd, "moisture")) { mock_moisture = val; printf("Moisture %.1f\n", val); }
            else if (!strcmp(cmd, "temp")) { mock_temp = val; printf("Temp %.1f\n", val); }
        } else if (!strcmp(line, "s") || !strcmp(line, "step")) {
            sim_millis += 1000;
            printf("Time now %lu ms\n", sim_millis);
        } else if (!strcmp(line, "q") || !strcmp(line, "quit")) break;
    }
    return 0;
}
