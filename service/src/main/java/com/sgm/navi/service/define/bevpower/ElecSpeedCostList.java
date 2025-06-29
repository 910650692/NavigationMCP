package com.sgm.navi.service.define.bevpower;


import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public class ElecSpeedCostList implements Parcelable {
    public int speed;
    public float costValue;

    public ElecSpeedCostList() {
        this.speed = 0;
        this.costValue = 0.0F;
    }

    public ElecSpeedCostList(int speedLiteObj, float costValueLiteObj) {
        this.speed = speedLiteObj;
        this.costValue = costValueLiteObj;
    }

    protected ElecSpeedCostList(Parcel in) {
        speed = in.readInt();
        costValue = in.readFloat();
    }

    public static final Creator<ElecSpeedCostList> CREATOR = new Creator<ElecSpeedCostList>() {
        @Override
        public ElecSpeedCostList createFromParcel(Parcel in) {
            return new ElecSpeedCostList(in);
        }

        @Override
        public ElecSpeedCostList[] newArray(int size) {
            return new ElecSpeedCostList[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(speed);
        dest.writeFloat(costValue);
    }
}
