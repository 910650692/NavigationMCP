package com.sgm.navi.service.define.aos;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyGTraEventDetail extends FyGSubTraEventDetail implements Parcelable {
    public FyGTraEventDetail() {
    }

    public int subdetailcount;// 有详情的子事件个数 （hasdetail=1，没有赋值的时候值为1） 非聚合事件为0
    public int subcount;//子事件个数 非聚合事件为0
    public ArrayList<FyGSubTraEventDetail> subinfo = new ArrayList<>();
    public boolean isRequestSuccess = false;
    public long taskId;

    protected FyGTraEventDetail(Parcel in) {
        subdetailcount = in.readInt();
        subcount = in.readInt();
        isRequestSuccess = in.readByte() != 0;
        taskId = in.readLong();
    }

    public static final Creator<FyGTraEventDetail> CREATOR = new Creator<FyGTraEventDetail>() {
        @Override
        public FyGTraEventDetail createFromParcel(Parcel in) {
            return new FyGTraEventDetail(in);
        }

        @Override
        public FyGTraEventDetail[] newArray(int size) {
            return new FyGTraEventDetail[size];
        }
    };

    public boolean hasChild() {
        return subcount > 0;
    }

    // 是否可以滚动
    public boolean canScroller() {
        return subcount > 1;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(subdetailcount);
        dest.writeInt(subcount);
        dest.writeByte((byte) (isRequestSuccess ? 1 : 0));
        dest.writeLong(taskId);
    }
}
