package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ETAInfo implements Parcelable {
    private int mDistance; //距离终点距离
    private String mTravelTime; //预计行程时间
    private long mTime; //预计行程时间
    private int mLeftCharge; //剩余电量

    public int getDistance() {
        return mDistance;
    }

    /**
     * 设置距离
     * @param distance 距离
     * @return ETAInfo
     */
    public ETAInfo setDistance(final int distance) {
        this.mDistance = distance;
        return this;
    }

    public String getTravelTime() {
        return mTravelTime;
    }

    /**
     * 设置旅行时间
     * @param travelTime 旅行时间
     * @return ETAInfo
     */
    public ETAInfo setTravelTime(final String travelTime) {
        this.mTravelTime = travelTime;
        return this;
    }

    public int getLeftCharge() {
        return mLeftCharge;
    }

    /**
     * 设置剩余电量
     * @param leftCharge 剩余电量
     * @return ETAInfo
     */
    public ETAInfo setLeftCharge(final int leftCharge) {
        this.mLeftCharge = leftCharge;
        return this;
    }

    /**
     * 设置剩余电量
     * @param time 剩余电量
     * @return ETAInfo
     */
    public ETAInfo setTime(final long time) {
        this.mTime = time;
        return this;
    }

    public long getTime() {
        return mTime;
    }

    protected ETAInfo(final Parcel in) {
       mDistance = in.readInt();
        mTravelTime = in.readString();
        mLeftCharge = in.readInt();
        mTime = in.readLong();
    }

    public static final Creator<ETAInfo> CREATOR = new Creator<ETAInfo>() {
        @Override
        public ETAInfo createFromParcel(final Parcel in) {
            return new ETAInfo(in);
        }

        @Override
        public ETAInfo[] newArray(final int size) {
            return new ETAInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mDistance);
        parcel.writeString(mTravelTime);
        parcel.writeInt(mLeftCharge);
        parcel.writeLong(mTime);
    }

    @Override
    public String toString() {
        return "ETAInfo{" +
                "mDistance=" + mDistance +
                ", mTravelTime='" + mTravelTime + '\'' +
                ", mTime=" + mTime +
                ", mLeftCharge=" + mLeftCharge +
                '}';
    }
}
