```markdown
# Model Train Control System (Programmeerproject 2)

An automated model train control system implemented in **Racket/Scheme**[cite: 3, 4]. The software operates a digital model railway system based on the **Z21 protocol** over a local network or via an integrated virtual simulator[cite: 3, 4].

---

## 🏗️ System Architecture

The project is built on a **Client-Server network architecture** using TCP communication (exchanging S-expressions) to enforce a strict separation between the user interface and track logic[cite: 3, 4]:

* **Infrabel (Server):** The core infrastructure engine[cite: 4]. It manages physical track elements (detection blocks, switches, signals, level crossings), computes automated routes, and guarantees network safety via a reservation system and proactive collision prevention[cite: 4].
* **NMBS (Client / GUI):** The traffic control interface built with the Racket GUI Toolkit[cite: 3, 4]. It acts as a remote proxy sending network messages to Infrabel while maintaining a local client cache (`Railway-NMBS`) to ensure smooth UI rendering without network lag[cite: 4].
* **Execution Facade:** A hardware abstraction layer enabling Infrabel to communicate interchangeably with either the virtual simulator or physical Z21 digital hardware over UDP[cite: 3, 4].

```text
[ NMBS Client / GUI ] <====== TCP (S-expressions) ======> [ Infrabel Server ]
   (Railway-NMBS Cache)                                     (Spoornetwerk & Safety)
                                                                       |
                                                                Execution Facade
                                                                       |
                                                      +----------------+----------------+
                                                      |                                 |
                                                      v                                 v
                                              Virtual Simulator               Z21 Hardware (UDP)

```

---

## ⚙️ Key System Functionalities

### 🚆 Train & Route Management

* **Speed & Direction Control:** Seamless speed adjustment supporting both forward and reverse motion.


* **Automated Routing:** Calculates the shortest path between a starting block and a destination using a **Breadth-First Traversal (BFT)** algorithm over a graph model of the railway network.


* **Automatic Turnaround:** Automatically detects and reverses train orientation when a route requires direction changes.



### 🔀 Infrastructure Control

* **Switches & Points:** Controls standard switches, 3-way switches (e.g., `S-2-3`), and double slip switches.


* **Level Crossings:** Automates crossing barriers (Open/Closed) with real-time state tracking.


* **Multi-Aspect Signals:** Supports signal states including `Hp0` (Red), `Hp1` (Green), `Ks1+Zs3`, `Oranje-Wit`, and other custom codes.



### 🛡️ Safety & Collision Prevention

* **Atomic Reservation System:** Claims all required track segments and switches atomically before a train executes a hop. If an element is reserved by another train, the request is blocked to prevent partial claims and collisions.


* **Active Monitoring & Emergency Stop:** A background thread continuously monitors occupied detection blocks. If two trains occupy the same block or a manually controlled train approaches an occupied block, an automatic **Emergency Stop** (`noodstop!`) is triggered, halting all trains and clearing all reservations.



### 💾 State Persistence (Scenarios)

* **Save & Load System:** Captures the real-time state of all trains, switches, signals, and crossing gates into a structured `.txt` file. Previous states can be loaded to restore exact railway configurations.



---

## 📐 Abstract Data Types (ADTs)

### Core Hardware ADTs

* **Train ADT (`trein.rkt`):** Tracks locomotive ID, speed, current and previous position pair, destination, and active routing thread handle.


* **Switch ADT (`wissel.rkt`):** Manages individual switch states.


* **Level Crossing ADT (`slagboom.rkt`):** Manages level crossing barrier positions (open/closed).


* **Signal ADT (`verkeerslicht.rkt`):** Tracks signal color codes (defaults to Red/`Hp0`).



### Infrastructure & Logic ADTs

* **Track Network ADT (`spoornetwerk.rkt`):** Models the physical track as a directed, labeled graph using the `a-d` graph library, where nodes represent detection blocks (`1-1` through `2-8`) and edges store necessary switch positions.


* **Reservation ADT (`reservatie.rkt`):** Thread-safe reservation engine using semaphores to claim or release track elements atomically.


* **Infrabel ADT (`infrabel.rkt`):** Central server coordinator uniting trains, switches, signals, the track graph, and the reservation system.


* **Execution Facade ADT (`execution-facade.rkt`):** Dispatches hardware commands dynamically to either the virtual simulator or Z21 network interface depending on runtime mode (`'SIM` or `'HARDWARE`).



### Client & Support ADTs

* **NMBS ADT (`nmbs.rkt`):** Network proxy client converting user interactions into RPC S-expressions over TCP.


* **Railway-NMBS ADT (`railway-nmbs.rkt`):** Client-side state cache storing track statuses for fast, local UI rendering.


* **Scenario ADT (`scenario.rkt`):** Manages file I/O operations for saving and restoring system state snapshots.



---

## 📁 Project Structure

```text
train-controller/
├── client.rkt               # Entry point for the NMBS Client GUI application[cite: 3, 4]
├── server.rkt               # Entry point for the Infrabel Infrastructure Server[cite: 3, 4]
├── gui.rkt                  # Tabbed user interface built with Racket GUI Toolkit[cite: 3, 4]
├── infrabel.rkt             # Core Infrabel Server ADT[cite: 4]
├── nmbs.rkt                 # RPC Client Proxy ADT[cite: 4]
├── railway-nmbs.rkt         # Local client state shadow-cache ADT[cite: 4]
├── spoornetwerk.rkt         # Track graph representation ADT[cite: 4]
├── reservatie.rkt           # Thread-safe reservation system ADT[cite: 4]
├── route.rkt                # Pure BFT route calculation module[cite: 4]
├── execution-facade.rkt     # Hardware/Simulator abstraction layer ADT[cite: 4]
├── scenario.rkt             # Save/Load file persistence ADT[cite: 4]
├── trein.rkt                # Locomotive model ADT[cite: 4]
├── wissel.rkt               # Switch/point model ADT[cite: 4]
├── slagboom.rkt             # Level crossing gate ADT[cite: 4]
├── verkeerslicht.rkt        # Railway signal model ADT[cite: 4]
├── constanten.rkt           # System constants and port configurations[cite: 3, 4]
├── simulator/               # Built-in track layout and simulator interface files[cite: 3]
└── hardware/                # Low-level Z21 UDP communication interface files[cite: 3]

```

---

## 🚀 Getting Started

### Prerequisites

1. Download and install **[DrRacket](https://racket-lang.org/)** (v9.0 or newer recommended).



### Running the Application

1. **Launch the Server (Infrabel):**
* Open `server.rkt` in DrRacket and click **Run**.


* Select the desired mode (**Simulator** or **Hardware (Z21)**) when prompted.




2. **Launch the Client (NMBS GUI):**
* Open `client.rkt` in a new DrRacket window and click **Run**.


* The NMBS control panel will open, allowing you to manage trains, switches, signals, and crossing barriers.





---
