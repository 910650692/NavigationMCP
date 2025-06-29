package com.sgm.navi.service.define.position;


import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SensorCalibrationPara implements Parcelable {
    private long startTime = -1;
    private long endTime = -1;
    private String para = "首次刷机，未开始标定";

    public SensorCalibrationPara(String para, long startTime, long endTime) {
        this.para = para;
        this.startTime = startTime;
        this.endTime = endTime;
    }

    protected SensorCalibrationPara(Parcel in) {
        startTime = in.readLong();
        endTime = in.readLong();
        para = in.readString();
    }

    public static final Creator<SensorCalibrationPara> CREATOR = new Creator<SensorCalibrationPara>() {
        @Override
        public SensorCalibrationPara createFromParcel(Parcel in) {
            return new SensorCalibrationPara(in);
        }

        @Override
        public SensorCalibrationPara[] newArray(int size) {
            return new SensorCalibrationPara[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeLong(startTime);
        dest.writeLong(endTime);
        dest.writeString(para);
    }
}
