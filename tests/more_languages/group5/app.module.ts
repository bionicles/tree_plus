import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClientModule } from '@angular/common/http';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { HomeComponent } from './home/home.component';
import { LoginComponent } from './login/login.component';
import { RegisterComponent } from './register/register.component';
import { EventsComponent } from './events/events.component';
import { InvitesComponent } from './invites/invites.component';
import { RewardsComponent } from './rewards/rewards.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatFormFieldModule } from '@angular/material/form-field';
import { NgxStripeModule } from 'ngx-stripe';
import { ProfileComponent } from './profile/profile.component';



@NgModule({
    declarations: [
        AppComponent,
        HomeComponent,
        LoginComponent,
        RegisterComponent,
        EventsComponent,
        InvitesComponent,
        RewardsComponent,
        ProfileComponent
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        AppRoutingModule,
        BrowserAnimationsModule,
        MatFormFieldModule,
        NgxStripeModule.forRoot('pk_test_is_completely_fake'),
    ],
    providers: [],
    bootstrap: [AppComponent]
})
export class AppModule { }
