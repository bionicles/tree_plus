import { TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';

import { AppComponent } from './app.component';

describe('AppComponent', () => {
    beforeEach(async () => {
        await TestBed.configureTestingModule({
            imports: [
                RouterTestingModule,
                HttpClientTestingModule
            ],
            declarations: [
                AppComponent
            ],
        }).compileComponents();
    });

    it('should create the app', () => {
        const fixture = TestBed.createComponent(AppComponent);
        const app = fixture.componentInstance;
        expect(app).toBeTruthy();
    });

    it("should welcome the user", () => {
      fixture.detectChanges();
      const content = el.textContent;
      expect(content).withContext('"Welcome ..."').toContain("Welcome");
      expect(content).withContext("expected name").toContain("Test User");
    });

    it('should welcome "Jimbo"', () => {
      userService.user.name = "Jimbo"; // welcome message hasn't been shown yet
      fixture.detectChanges();
      expect(el.textContent).toContain("Jimbo");
    });

    it("should request login if not logged in", () => {
      userService.isLoggedIn = false; // welcome message hasn't been shown yet
      fixture.detectChanges();
      const content = el.textContent;
      expect(content).withContext("not welcomed").not.toContain("Welcome");
      expect(content)
        .withContext('"log in"')
        .toMatch(/log in/i);
    });
});