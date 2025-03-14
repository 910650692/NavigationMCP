package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.Date;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: FavoriteInfo:收藏信息
 * @CreateDate: 2025/2/12 13:33
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class FavoriteInfo implements Parcelable {
    private String itemId;         // 收藏点唯一码, 由AutoSDK内部生成
    private int commonName;        // 收藏点类型（1家，2公司，0普通收藏点）
    private String tag;            // 附加标签
    private String type;           // 类型
    private String newType;        //  新类型，预留
    private String custom_name;    // 自定义名称 重命名时编辑的字段
    private String classification; //  类别
    private long top_time;         // 置顶操作内部更新时间
    private long updateTime;
    protected FavoriteInfo(Parcel in) {
        itemId = in.readString();
        commonName = in.readInt();
        tag = in.readString();
        type = in.readString();
        newType = in.readString();
        custom_name = in.readString();
        classification = in.readString();
        top_time = in.readLong();
        updateTime = in.readLong();
    }

    public static final Creator<FavoriteInfo> CREATOR = new Creator<FavoriteInfo>() {
        @Override
        public FavoriteInfo createFromParcel(Parcel in) {
            return new FavoriteInfo(in);
        }

        @Override
        public FavoriteInfo[] newArray(int size) {
            return new FavoriteInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(itemId);
        parcel.writeInt(commonName);
        parcel.writeString(tag);
        parcel.writeString(type);
        parcel.writeString(newType);
        parcel.writeString(custom_name);
        parcel.writeString(classification);
        parcel.writeLong(top_time);
        parcel.writeLong(updateTime);
    }
}
