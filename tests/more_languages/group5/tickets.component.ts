import {
  Component,
  Input,
  Output,
  EventEmitter,
  AfterViewInit,
  ViewChildren,
  QueryList,
  HostListener,
  Renderer2,
  ElementRef,
} from "@angular/core";
import { Sort } from "@angular/material/sort";
import { faker } from "@faker-js/faker";
import { MatOptionSelectionChange } from "@angular/material/core";
import { MatDatepickerInputEvent } from "@angular/material/datepicker";
import * as geolib from "geolib";
const QRCode = require("qrcode");

import { Subject } from "rxjs";
import { debounceTime } from "rxjs/operators";

// 9 september 2023 - make sure it's up to date!
import {
  Ticket,
  TicketStatusEnum,
  RoleEnum,
  Location,
  Event as PromoEvent,
} from "../models";

const DEFAULT_NUM_DAYS_TO_SHOW = 100;

// export interface Ticket {
//   ticket_id?: number;
//   buyer_id: number;
//   guest_id?: number;
//   event_id: number;
//   payment_id: string;
//   paid: boolean;
//   qr_code_key: string;
//   qr_code: Buffer;
//   ticket_status: TicketStatusEnum;
//   commission_percent_paid: number;
//   refund_percent_paid: number;
//   drinks_redeemed: number;
//   price: number;
//   role: RoleEnum;
//   inviter_id?: number;
//   created_at: Date;
//   updated_at: Date;
// }

// enriched tickets joined to event and location objects
interface EnrichedTicket extends Ticket {
  event?: PromoEvent;
  location?: Location;
  // Any other enriched properties
}

// configuration for the spinning animation on qr codes
interface SpinConfig {
  transitionDuration: number; // duration of spin
  decelerationCurve: string; // easing function for deceleration
  minRotation: number;
  maxRotation: number;
  scaleFactor: number;
  //... add other needed configs
}

const DEFAULT_SPIN_CONFIG: SpinConfig = {
  transitionDuration: 200, // e.g., 0.3s
  decelerationCurve: "cubic-bezier(.17,.67,.83,.67)",
  minRotation: 90, // min quarter turn
  maxRotation: 360 + 90, // max one and one quarter turn
  scaleFactor: 1.09, // scale up by 9%
};

interface RotationState {
  direction: 1 | -1;
  rotation: number;
  scale: number;
  isHovered: boolean;
  isAnimating?: boolean;
}

interface SpeakInput {
  key: string;
  value: string | Date;
}

const formatSpeakInput = (input: SpeakInput): string => {
  return `${input.key} ${input.value}`;
};

function hourToSpeech(hour: number, minute: number, period: string): string {
  const adjustedHour = hour % 12 || 12;
  const adjustedMinute = minute === 0 ? "00" : `${minute}`;
  let speech = `${adjustedHour}:${adjustedMinute} ${period}`;
  return speech;
}

@Component({
  selector: "app-tickets",
  templateUrl: "./tickets.component.html",
  styleUrls: ["./tickets.component.scss"],
})
export class TicketsComponent implements AfterViewInit {
  // read qr code keys aloud when clicked
  // Initialize speech synthesis
  speechSynthesisInstance = new SpeechSynthesisUtterance();
  currentSpeakInput: SpeakInput | null = null;

  speak(input: SpeakInput) {
    console.log("speak input", input);
    // Cancel any ongoing speech
    window.speechSynthesis.cancel();

    // Check if clicked key is the same as the one being read
    if (this.currentSpeakInput == input) {
      this.speechSynthesisInstance.text = formatSpeakInput(input);
      window.speechSynthesis.speak(this.speechSynthesisInstance);
      return;
    }

    // Update the key being read
    this.currentSpeakInput = input;

    // Configure the SpeechSynthesisUtterance instance
    this.speechSynthesisInstance.text = formatSpeakInput(input);
    this.speechSynthesisInstance.lang = "en-US";
    this.speechSynthesisInstance.pitch = 1; // 0 to 2
    this.speechSynthesisInstance.rate = 1; // 0.1 to 10
    this.speechSynthesisInstance.volume = 1; // 0 to 1
    // Read the key aloud
    window.speechSynthesis.speak(this.speechSynthesisInstance);
  }

  // export interface Event {
  //   event_id?: number;
  //   name: string;
  //   date: Date;
  //   start_time: Date;
  //   end_time: Date;
  //   performer: string;
  //   lineup: string[];
  //   rewards?: Record<string, unknown>;
  //   tier_ids?: number[];
  //   location_ids: number[];
  //   created_at?: Date;
  //   updated_at?: Date;
  // }
  speakEvent(ticket: EnrichedTicket): void {
    const key = `Event ${ticket.event_id}:`;
    const value = `${ticket.event?.name} with ${ticket.event?.performer}`;
    this.speak({ key, value });
  }
  formatEvent(ticket: EnrichedTicket): string {
    const one = `Event ${ticket.event_id}:`;
    const two = `${ticket.event?.name}`;
    const three = `${ticket.event?.performer}`;
    return `${one}<br>${two}<br>with<br>${three}`;
  }

  speakVenue(ticket: EnrichedTicket): void {
    const key = `Event Venue Location: ${ticket.location?.name}. ${ticket.location?.address}. ...`;
    const value = `Latitude: ${ticket.location?.latitude}; Longitude: ${ticket.location?.longitude}`;
    this.speak({ key, value });
  }

  // Function to format date for visual and auditory representation
  formatDate(date: Date, oneLiner: boolean = false): string {
    const dateTimeFormat = new Intl.DateTimeFormat("en-US", {
      timeZone: date.getTimezoneOffset() === 0 ? "UTC" : undefined,
      year: "numeric",
      month: "long",
      day: "numeric",
      timeZoneName: "short",
      hour: "2-digit",
      minute: "2-digit",
      hour12: true,
    });

    const parts = dateTimeFormat.formatToParts(date);
    const dateParts = parts.filter((part) =>
      ["day", "month", "year"].includes(part.type)
    );
    const timeParts = parts.filter((part) =>
      ["hour", "minute", "dayPeriod", "timeZoneName"].includes(part.type)
    );

    const day = dateParts.find((p) => p.type === "day")?.value;
    const month = dateParts.find((p) => p.type === "month")?.value;
    const year = dateParts.find((p) => p.type === "year")?.value;
    const dateString = [month, day, year].join(" ");

    const hour = parseInt(
      timeParts.find((part) => part.type === "hour")?.value || "0",
      10
    );
    const minute = parseInt(
      timeParts.find((part) => part.type === "minute")?.value || "0",
      10
    );
    const period =
      timeParts.find((part) => part.type === "dayPeriod")?.value || "";
    const timeZone =
      timeParts.find((part) => part.type === "timeZoneName")?.value || "";

    const timeString = `${hourToSpeech(hour, minute, period)} ${timeZone}`;

    return oneLiner
      ? `${dateString}, ${timeString}`
      : `${dateString}<br>${timeString}`;
  }

  // Updated formatDateForSpeech function
  formatDateForSpeech(date: Date): string {
    const formattedString = this.formatDate(date, true);
    const dateAndTime = formattedString.split(", "); // ["August 1 2023", "06:00 PM EDT"]

    return `${dateAndTime[0]}, ${dateAndTime[1]}`;
  }

  // spin qr codes on click
  // multiple image elements per component (in different rows of this table)
  @ViewChildren("qrCodeImage") qrImages?: QueryList<ElementRef>;

  // Track hover status so when animation ends we can decide if to shrink
  private hoverListeners: Map<HTMLElement, (() => void)[]> = new Map();

  // This map will hold the current rotation state for each image
  private rotationsMap: Map<HTMLElement, RotationState> = new Map();

  async spinQRCode(
    event: PointerEvent,
    config: SpinConfig = DEFAULT_SPIN_CONFIG
  ) {
    const imgElement: HTMLElement = event.target as HTMLElement;
    let state: RotationState = this.rotationsMap.get(imgElement) || {
      direction: 1,
      rotation: 0,
      scale: config.scaleFactor,
      isHovered: true, // assuming clicks hover
    };
    state.isAnimating = true; // Set the animating flag
    this.rotationsMap.set(imgElement, state);

    const computedTransform = getComputedStyle(imgElement).transform;
    const currentRotation = this.getCurrentRotation(computedTransform);

    // Calculate nearest 90-degree rotation within the provided range.
    let randomRotation =
      state.direction *
      (Math.random() * (config.maxRotation - config.minRotation) +
        config.minRotation);
    let rotationValue = this.getNext90Degree(currentRotation + randomRotation);

    state.rotation = rotationValue;
    state.direction *= -1;
    this.rotationsMap.set(imgElement, state);

    this.animateRotation(imgElement, rotationValue, config, () => {
      state.isAnimating = false;
      // disabled scale factor for simplicity (only hover scales)
      // const scaleValue = state?.isHovered ? config.scaleFactor : 1;
      this.renderer.setStyle(
        imgElement,
        "transform",
        `rotate(${rotationValue}deg)`
      );
      this.rotationsMap.set(imgElement, state);
    });
  }
  private animateRotation(
    imgElement: HTMLElement,
    targetRotation: number,
    config: SpinConfig,
    cleanup: () => void
  ) {
    const startTime = performance.now();
    const initialTransform = getComputedStyle(imgElement).transform;
    const initialRotation = this.getCurrentRotation(initialTransform);

    const animate = (currentTime: number) => {
      const elapsed = currentTime - startTime;
      const progress = Math.min(elapsed / config.transitionDuration, 1);

      const currentRotation =
        initialRotation + progress * (targetRotation - initialRotation);

      this.renderer.setStyle(
        imgElement,
        "transform",
        `rotate(${currentRotation}deg)`
      );

      if (progress < 1) {
        requestAnimationFrame(animate);
      } else {
        cleanup();
      }
    };

    requestAnimationFrame(animate);
  }

  private getNext90Degree(currentRotation: number): number {
    return Math.ceil(currentRotation / 90) * 90;
  }

  private getCurrentRotation(matrix: string): number {
    // Assuming a 2D transform matrix, extract the angle.
    const values = matrix.split("(")[1].split(")")[0].split(", ");
    const a = +values[0];
    const b = +values[1];
    const angle = Math.round(Math.atan2(b, a) * (180 / Math.PI));
    return angle < 0 ? angle + 360 : angle;
  }

  // Lifecycle for hover listeners
  ngAfterViewInit() {
    // Now this.qrImages contains all your img tags.
    // If you want to iterate over them:
    this.qrImages?.forEach((imgRef) => {
      // 'imgElement' is declared but its value is never read.ts(6133)
      const imgElement = imgRef.nativeElement;
      // Now you can work with the imgElement directly
      // Mouse enter listener
      const mouseEnterListener = () => {
        let state = this.rotationsMap.get(imgElement) || {
          direction: 1,
          rotation: 0,
          scale: DEFAULT_SPIN_CONFIG.scaleFactor,
          isHovered: true,
        };
        state.isHovered = true;
        this.rotationsMap.set(imgElement, state);
      };

      // Mouse leave listener
      const mouseLeaveListener = () => {
        let state = this.rotationsMap.get(imgElement) || {
          direction: 1,
          rotation: 0,
          scale: DEFAULT_SPIN_CONFIG.scaleFactor,
          isHovered: false,
        };
        state.isHovered = false;
        if (!state.isAnimating) {
          // Check if the animation isn't running
          this.renderer.setStyle(
            imgElement,
            "transform",
            `rotate(${state.rotation}deg) scale(1)`
          );
        }
        this.rotationsMap.set(imgElement, state);
      };

      // Attach listeners
      imgElement.addEventListener("mouseenter", mouseEnterListener);
      imgElement.addEventListener("mouseleave", mouseLeaveListener);

      // Store the listeners to remove them later
      this.hoverListeners.set(imgElement, [
        mouseEnterListener,
        mouseLeaveListener,
      ]);
    });
  }

  // End hover listener lifecycle
  ngOnDestroy() {
    // Remove the listeners upon the component's destruction
    this.hoverListeners.forEach((listeners, imgElement) => {
      listeners.forEach((listener) => {
        imgElement.removeEventListener("mouseenter", listener);
        imgElement.removeEventListener("mouseleave", listener);
      });
    });
  }

  // Table columns
  // this one decides what's possible to turn on:
  possibleColumns: string[] = [
    "venue",
    "start_time",
    "created_at",
    "event_id",
    "guest_id",
    "ticket_id",
    "buyer_id",
    "inviter_id",
    "qr_code",
    "payment_id",
    "ticket_status",
    "role",
    "price",
    "refund_percent_paid",
    "drinks_redeemed",
    "commission_percent_paid",
    "updated_at",
  ];
  // initialization & resize events select these columns:
  defaultColumnsMobile: string[] = [
    "venue",
    "start_time",
    "event_id",
    "qr_code",
  ];
  defaultColumnsDesktop: string[] = [
    "venue",
    "start_time",
    "event_id",
    "qr_code",
    "ticket_status",
    "role",
    "drinks_redeemed",
    "commission_percent_paid",
  ];
  displayedColumns: string[] = [];

  // enable manual toggling of columns in the select columns input at the top
  toggleColumn(event: MatOptionSelectionChange, column: string) {
    if (event.isUserInput) {
      if (this.displayedColumns.includes(column)) {
        this.displayedColumns = this.displayedColumns.filter(
          (col) => col !== column
        );
      } else {
        this.displayedColumns.push(column);
      }
    }
  }

  // enable nice mobile ux: switch to different columns for narrow view width automatically
  private lastIsMobile: boolean | null = null; // Track the last known state
  private resizeEvent = new Subject<Event>();
  adjustColumns(event?: Event) {
    const windowWidth = event
      ? (event.target as Window).innerWidth
      : window.innerWidth;
    this.isMobile = windowWidth < 768;
    // only update if the state changed
    if (this.lastIsMobile === null || this.lastIsMobile !== this.isMobile) {
      this.lastIsMobile = this.isMobile; // Update the last known state
      this.displayedColumns = this.isMobile
        ? this.defaultColumnsMobile
        : this.defaultColumnsDesktop;
    }
  }
  @HostListener("window:resize", ["$event"])
  onResize(event: Event) {
    this.adjustColumns(event);
  }

  @Input() tickets: EnrichedTicket[] = [];
  originalTickets: EnrichedTicket[] = [];
  @Input() role: RoleEnum = RoleEnum.guest;
  isMobile: boolean = false;
  async ngOnInit() {
    // Initial adjustment of columns
    this.adjustColumns();

    // listen for and debounce window resize events so we don't adjust our columns too often
    this.resizeEvent
      .pipe(
        debounceTime(300) // milliseconds
      )
      .subscribe((event) => {
        this.adjustColumns(event);
      });
    // define the empty filter for each column in a loop
    this.possibleColumns.forEach((col) => (this.columnFilters[col] = ""));
    await this.loadTickets();

    // set the latitude and longitude to Charlotte, North Carolina
    console.log("setting default location to Charlotte, North Carolina");
    this.latitude = 35.2271;
    this.longitude = -80.8431;

    const currentDate = new Date();
    this.startDate = new Date(currentDate);
    this.endDate = new Date(currentDate);
    this.endDate.setDate(this.endDate.getDate() + DEFAULT_NUM_DAYS_TO_SHOW);
    this.applyFilter("start_time"); // apply the default date filter
  }
  // original load tickets function
  // async loadTickets(): Promise<void> {
  //   const fetchedTickets = await mockTickets();
  //   this.tickets = [...fetchedTickets];
  //   this.originalTickets = [...fetchedTickets];
  // }

  // new load tickets function, enriches them with the event and location objects
  async loadTickets(): Promise<void> {
    const fetchedTickets: EnrichedTicket[] = await mockTickets();
    fetchedTickets.forEach((ticket) => {
      ticket.event = mockEvents[ticket.event_id];
      ticket.location =
        mockLocations[mockEvents[ticket.event_id]?.location_ids[0]]; // Assuming only one location for simplicity
    });
    this.tickets = fetchedTickets;
    this.originalTickets = [...fetchedTickets];
  }

  // Filtering via direct column header inputs
  columnFilters: { [key: string]: string } = {};
  // spatial filter variables
  latitude: number | null = null;
  longitude: number | null = null;
  radius_miles: number | null = null;
  // temporal filter variables
  startDate: Date | null = null; // default to future events!
  endDate: Date | null = null;
  onDateRangeChange(
    type: "start" | "end",
    event: MatDatepickerInputEvent<Date>
  ) {
    if (type === "start") {
      this.startDate = event.value;
    } else if (type === "end") {
      this.endDate = event.value;
    }
    // Validate both dates are present before applying filter
    if (this.startDate && this.endDate) {
      this.applyFilter("start_time");
    }
  }
  applyFilter(column: string): void {
    const filterValue = this.columnFilters[column];
    if (column === "venue") {
      if (
        isNonNullNumber(this.latitude) &&
        isNonNullNumber(this.longitude) &&
        isNonNullNumber(this.radius_miles)
      ) {
        // Since we passed the type guard, TypeScript knows these are numbers.
        const { latitude, longitude, radius_miles } = this;
        console.log("filtering by venue", {
          latitude,
          longitude,
          radius_miles,
        });
        this.tickets = this.originalTickets.filter((ticket) => {
          // Using our custom type guard for ticket location
          if (hasLocation(ticket)) {
            return geolib.isPointWithinRadius(
              { lat: ticket.location.latitude, lng: ticket.location.longitude },
              { lat: latitude, lng: longitude },
              radius_miles * 1609.34 // convert miles to meters
            );
          }
          return false;
        });
      }
      return;
    }
    if (column === "start_time") {
      if (this.startDate !== null && this.endDate !== null) {
        const { startDate, endDate } = this;
        console.log("filtering by start_time", { startDate, endDate });
        if (!isDate(startDate) || !isDate(endDate)) {
          console.error("startDate or endDate is not a date");
          return;
        }
        let keepers = [];
        for (let ticket of this.originalTickets) {
          const eventBegins = ticket.event?.start_time;
          if (!isDate(eventBegins)) {
            console.error("eventBegins is not a date");
            return;
          } else {
            if (eventBegins >= startDate && eventBegins <= endDate) {
              // console.log('keep', ticket);
              keepers.push(ticket);
            } else {
              // console.log('discard', ticket);
            }
          }
        }
        this.tickets = keepers;
        return;
      }
    }
    this.tickets = this.originalTickets.filter((ticket) => {
      if (filterValue) {
        // Special handling for date columns
        if (column === "created_at" || column === "updated_at") {
          // console.log('filtering date column and filterValue', column, filterValue);
          // Convert both to a standard format string for comparison
          return this.formatDateForComparison(ticket[column]).includes(
            filterValue
          );
        }
        // For other columns
        return String((ticket as any)[column])
          .toLowerCase()
          .includes(filterValue.toLowerCase());
      }
      return true; // if no filter, return true
    });
  }

  // Utility function to format dates for comparison
  formatDateForComparison(date: Date): string {
    const options: Intl.DateTimeFormatOptions = {
      year: "numeric",
      month: "2-digit",
      day: "2-digit",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      timeZoneName: "short",
    };
    return new Intl.DateTimeFormat("default", options).format(date);
  }

  // Debounce filtering to avoid unnecessary looping during typing
  // Create a subject for each filter input
  private filterChange = new Subject<{ value: string; column: string }>();

  constructor(private renderer: Renderer2) {
    // Listen for filter changes and debounce them
    this.filterChange
      .pipe(
        debounceTime(200) // milliseconds
      )
      .subscribe(({ value, column }) => {
        this.columnFilters[column] = value;
        this.applyFilter(column);
      });
  }

  onFilterChange(event: Event, column: string) {
    const input = event.target as HTMLInputElement;
    this.filterChange.next({ value: input.value, column });
  }
  // location changes:
  onLatitudeChange(event: Event) {
    console.log("onLatitudeChange event", event);
    this.latitude = parseFloat((event.target as HTMLInputElement).value);
    return;
  }
  onLongitudeChange(event: Event) {
    console.log("onLongitudeChange event", event);
    this.longitude = parseFloat((event.target as HTMLInputElement).value);
    return;
  }
  onRadiusChange(event: Event) {
    console.log("onRadiusChange event", event);
    this.radius_miles = parseFloat((event.target as HTMLInputElement).value);
    if (this.longitude && this.latitude && this.radius_miles) {
      this.applyFilter("venue");
    }
    return;
  }

  sortData(sort: Sort): void {
    const data = this.tickets.slice();
    if (!sort.active || sort.direction === "") {
      this.tickets = data;
      return;
    }
    this.tickets = data.sort((a, b) => {
      const isAsc = sort.direction === "asc";
      switch (sort.active) {
        case "venue":
          if (
            isNonNullNumber(this.latitude) &&
            isNonNullNumber(this.longitude)
          ) {
            // Latitude and Longitude are now known to be non-null numbers
            const { latitude, longitude } = this;

            return compare(
              hasLocation(a)
                ? geolib.getDistance(
                    { lat: a.location.latitude, lng: a.location.longitude },
                    { lat: latitude, lng: longitude }
                  )
                : Infinity,
              hasLocation(b)
                ? geolib.getDistance(
                    { lat: b.location.latitude, lng: b.location.longitude },
                    { lat: latitude, lng: longitude }
                  )
                : Infinity,
              isAsc
            );
          }
          return 0;
        case "event_id":
          return compare(a.event_id, b.event_id, isAsc);
        case "buyer_id":
          return compare(a.buyer_id, b.buyer_id, isAsc);
        case "guest_id":
          return compare(a.guest_id || 0, b.guest_id || 0, isAsc);
        case "ticket_id":
          return compare(a.ticket_id || 0, b.ticket_id || 0, isAsc);
        case "qr_code":
          return compare(a.qr_code_key, b.qr_code_key, isAsc);
        case "payment_id":
          return compare(a.payment_id, b.payment_id, isAsc);
        case "ticket_status":
          return compare(a.ticket_status, b.ticket_status, isAsc);
        case "role":
          return compare(a.role, b.role, isAsc);
        case "price":
          return compare(a.price, b.price, isAsc);
        case "commission_percent_paid":
          return compare(
            a.commission_percent_paid,
            b.commission_percent_paid,
            isAsc
          );
        case "refund_percent_paid":
          return compare(a.refund_percent_paid, b.refund_percent_paid, isAsc);
        case "drinks_redeemed":
          return compare(a.drinks_redeemed, b.drinks_redeemed, isAsc);
        case "inviter_id":
          return compare(a.inviter_id || 0, b.inviter_id || 0, isAsc);
        case "start_time":
          let a_event_start_time = a.event?.start_time || new Date();
          let b_event_start_time = b.event?.start_time || new Date();
          return compare_dates(a_event_start_time, b_event_start_time, isAsc);
        case "created_at":
          return compare_dates(a.created_at, b.created_at, isAsc);
        case "updated_at":
          return compare_dates(a.updated_at, b.updated_at, isAsc);
        default:
          return 0;
      }
    });
  }

  @Output() showTicket = new EventEmitter<Ticket>();
  @Output() showScanner = new EventEmitter<void>();
  onRowClick(event: Event, row: any) {
    // Identify original event target
    const originalTarget: HTMLElement = event.target as HTMLElement;

    // Check if the target is the QR code key span
    if (originalTarget && originalTarget.classList.contains("qr-code-key")) {
      // You can either do nothing or execute the speech logic here
      return;
    }

    // Rest of your onRowClick logic
    // show scanners for creator, venue agent, admin, and crew
    console.log("TODO! show ticket or scanner on row click");
    // if (
    //   this.role in
    //   [RoleEnum.creator, RoleEnum.venue_agent, RoleEnum.admin, RoleEnum.crew]
    // ) {
    //   this.showScanner.emit();
    // } else {
    //   this.showTicket.emit(ticket);
    // }
  }
}

// Type Guard Functions
function isDate(value: Date | undefined | null): value is Date {
  return value !== null && value !== undefined;
}

function isNonNullNumber(value: number | null): value is number {
  return value !== null;
}

function hasLocation(
  ticket: any
): ticket is { location: { latitude: number; longitude: number } } {
  return (
    ticket &&
    ticket.location &&
    typeof ticket.location.latitude === "number" &&
    typeof ticket.location.longitude === "number"
  );
}

// 10 september 2023 - make sure it's up to date!
const create_faker_ticket = async () => {
  let qrText = faker.string.uuid();
  let qrCode = await renderQRCode(qrText);
  let faker_ticket: Ticket = {
    ticket_id: faker.number.int(),
    buyer_id: faker.number.int(),
    guest_id: faker.number.int(),
    event_id: faker.number.int(),
    payment_id: faker.string.uuid(),
    paid: faker.datatype.boolean(),
    qr_code_key: qrText,
    qr_code: qrCode,
    ticket_status: faker.helpers.arrayElement(Object.values(TicketStatusEnum)),
    commission_percent_paid: faker.number.int({ min: 0, max: 100 }),
    refund_percent_paid: faker.number.int({ min: 0, max: 100 }),
    drinks_redeemed: faker.number.int(),
    price: faker.number.int(),
    role: faker.helpers.arrayElement(Object.values(RoleEnum)),
    inviter_id: faker.number.int(),
    created_at: faker.date.recent(),
    updated_at: faker.date.recent(),
  };
  return faker_ticket;
};

function compare(a: number | string, b: number | string, isAsc: boolean) {
  return (a < b ? -1 : 1) * (isAsc ? 1 : -1);
}
function compare_dates(a: Date, b: Date, isAsc: boolean) {
  return (a.getTime() < b.getTime() ? -1 : 1) * (isAsc ? 1 : -1);
}

async function mockMoreTickets(): Promise<Ticket[]> {
  let fake_tickets: Ticket[] = [];
  for (let i = 0; i < 100; i++) {
    let fake_ticket = await create_faker_ticket();
    fake_tickets.push(fake_ticket);
  }
  return fake_tickets;
}

// fake table joins so we can test the space-time filters
const mockEvents: { [key: number]: PromoEvent } = {
  101: {
    event_id: 102,
    name: "Robotic Trance",
    date: new Date("2023-09-20T00:00:00Z"),
    start_time: new Date("September 20 2023 10:00 PM EDT"),
    end_time: new Date("2023-09-21T02:00:00Z"),
    performer: "DJ Bionicle",
    lineup: ["Act 1", "Act 2"],
    location_ids: [1],
  },
};
const mockLocations: { [key: number]: Location } = {
  1: {
    location_id: 1,
    name: "Hilton Head Island Beach",
    address: "42 Palmetto Plaza",
    latitude: 32.2163,
    longitude: -80.7526,
  },
  2: {
    location_id: 2,
    name: "Charlotte Music Hall",
    address: "1911 Central Ave",
    latitude: 35.2271,
    longitude: -80.8431,
  },
};

const mockTickets = async () => [
  {
    // creator
    ticket_id: 1,
    buyer_id: 3,
    event_id: 101,
    payment_id: "PAY_123456",
    paid: true,
    qr_code_key: "QR_1A2B3C",
    qr_code: await renderQRCode("QR_1A2B3C"),
    ticket_status: TicketStatusEnum.ready_to_party,
    commission_percent_paid: 20,
    refund_percent_paid: 0,
    drinks_redeemed: 3,
    price: 0,
    role: RoleEnum.creator,
    inviter_id: 10,
    created_at: new Date("2023-08-01T10:00:00Z"),
    updated_at: new Date("2023-08-01T12:00:00Z"),
  },
  {
    ticket_id: 8,
    buyer_id: 8,
    guest_id: 35,
    event_id: 102,
    payment_id: "PAY_222324",
    paid: true,
    qr_code_key: "QR_M5N6O7",
    qr_code: await renderQRCode("QR_V5W6X7"),
    ticket_status: TicketStatusEnum.scanned,
    role: RoleEnum.guest,
    commission_percent_paid: 0,
    refund_percent_paid: 0,
    drinks_redeemed: 0,
    price: 99,
    created_at: new Date("2023-09-21T18:00:00Z"),
    updated_at: new Date("2023-09-21T20:00:00Z"),
  },
];

const renderQRCode = async (text: String): Promise<string> => {
  try {
    return await QRCode.toDataURL(text);
  } catch (err) {
    console.error(err);
    return "";
  }
};
