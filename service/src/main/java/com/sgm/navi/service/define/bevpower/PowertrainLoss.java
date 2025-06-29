package com.sgm.navi.service.define.bevpower;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public class PowertrainLoss implements Parcelable {
    public float powerdemand;
    public float costValue;

    public PowertrainLoss() {
        this.powerdemand = 0.0F;
        this.costValue = 0.0F;
    }

    public PowertrainLoss(float powerdemandLiteObj, float costValueLiteObj) {
        this.powerdemand = powerdemandLiteObj;
        this.costValue = costValueLiteObj;
    }

    protected PowertrainLoss(Parcel in) {
        powerdemand = in.readFloat();
        costValue = in.readFloat();
    }

    public static final Creator<PowertrainLoss> CREATOR = new Creator<PowertrainLoss>() {
        @Override
        public PowertrainLoss createFromParcel(Parcel in) {
            return new PowertrainLoss(in);
        }

        @Override
        public PowertrainLoss[] newArray(int size) {
            return new PowertrainLoss[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeFloat(powerdemand);
        dest.writeFloat(costValue);
    }
}
