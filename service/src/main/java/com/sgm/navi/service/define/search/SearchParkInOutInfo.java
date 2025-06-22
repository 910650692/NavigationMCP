package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 停车场出入口
 * @CreateDate: $ $
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SearchParkInOutInfo implements Parcelable {
    private double mX;
    private double mY;
    private String mKeytype;
    private String mEntExitId;

    public double getX() {
        return mX;
    }

    /**
     * 设置经度
     * @param x 经度
     * @return SearchParkInOutInfo
     */
    public SearchParkInOutInfo setX(final double x) {
        this.mX = x;
        return this;
    }

    public double getY() {
        return mY;
    }

    /**
     * 设置纬度
     * @param y 纬度
     * @return SearchParkInOutInfo
     */
    public SearchParkInOutInfo setY(final double y) {
        this.mY = y;
        return this;
    }

    public String getKeytype() {
        return mKeytype;
    }

    /**
     * 设置出入口类型
     * @param keytype 出入口类型
     * @return SearchParkInOutInfo
     */
    public SearchParkInOutInfo setKeytype(final String keytype) {
        this.mKeytype = keytype;
        return this;
    }

    public String getEntExitId() {
        return mEntExitId;
    }

    /**
     * 设置出入口Id
     * @param entExitId 出入口Id
     * @return SearchParkInOutInfo
     */
    public SearchParkInOutInfo setEntExitId(final String entExitId) {
        this.mEntExitId = entExitId;
        return this;
    }

    protected SearchParkInOutInfo(final Parcel in) {
        mX = in.readDouble();
        mY = in.readDouble();
        mKeytype = in.readString();
        mEntExitId = in.readString();
    }

    public static final Creator<SearchParkInOutInfo> CREATOR = new Creator<SearchParkInOutInfo>() {
        @Override
        public SearchParkInOutInfo createFromParcel(final Parcel in) {
            return new SearchParkInOutInfo(in);
        }

        @Override
        public SearchParkInOutInfo[] newArray(final int size) {
            return new SearchParkInOutInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeDouble(mX);
        parcel.writeDouble(mY);
        parcel.writeString(mKeytype);
        parcel.writeString(mEntExitId);
    }
}
