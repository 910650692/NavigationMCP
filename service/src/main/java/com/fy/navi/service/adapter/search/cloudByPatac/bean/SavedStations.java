package com.fy.navi.service.adapter.search.cloudByPatac.bean;
import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SavedStations implements Parcelable {
    @SerializedName("operatorId")
    private String mOperatorId;
    @SerializedName("stationId")
    private String mStationId;
    @SerializedName("stationSaved")
    private boolean mStationSaved;
    @SerializedName("lastUpdateTime")
    private long mLastUpdateTime;

    public String getmOperatorId() {
        return mOperatorId;
    }

    public SavedStations setmOperatorId(String mOperatorId) {
        this.mOperatorId = mOperatorId;
        return this;
    }

    public String getmStationId() {
        return mStationId;
    }

    public SavedStations setmStationId(String mStationId) {
        this.mStationId = mStationId;
        return this;
    }

    public boolean ismStationSaved() {
        return mStationSaved;
    }

    public SavedStations setmStationSaved(boolean mStationSaved) {
        this.mStationSaved = mStationSaved;
        return this;
    }

    public long getmLastUpdateTime() {
        return mLastUpdateTime;
    }

    public SavedStations setmLastUpdateTime(long mLastUpdateTime) {
        this.mLastUpdateTime = mLastUpdateTime;
        return this;
    }

    protected SavedStations(Parcel in) {
        mOperatorId = in.readString();
        mStationId = in.readString();
        mStationSaved = in.readBoolean();
        mLastUpdateTime = in.readLong();
    }

    public static final Creator<SavedStations> CREATOR = new Creator<SavedStations>() {
        @Override
        public SavedStations createFromParcel(Parcel in) {
            return new SavedStations(in);
        }

        @Override
        public SavedStations[] newArray(int size) {
            return new SavedStations[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(mOperatorId);
        parcel.writeString(mStationId);
        parcel.writeBoolean(mStationSaved);
        parcel.writeLong(mLastUpdateTime);
    }
}
