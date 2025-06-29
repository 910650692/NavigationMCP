package com.sgm.navi.service.define.bevpower;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public class ElecCommonParameter implements Parcelable {
    public float access;
    public float decess;

    public ElecCommonParameter() {
        this.access = 0.0F;
        this.decess = 0.0F;
    }

    public ElecCommonParameter(float accessLiteObj, float decessLiteObj) {
        this.access = accessLiteObj;
        this.decess = decessLiteObj;
    }

    protected ElecCommonParameter(Parcel in) {
        access = in.readFloat();
        decess = in.readFloat();
    }

    public static final Creator<ElecCommonParameter> CREATOR = new Creator<ElecCommonParameter>() {
        @Override
        public ElecCommonParameter createFromParcel(Parcel in) {
            return new ElecCommonParameter(in);
        }

        @Override
        public ElecCommonParameter[] newArray(int size) {
            return new ElecCommonParameter[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeFloat(access);
        dest.writeFloat(decess);
    }
}