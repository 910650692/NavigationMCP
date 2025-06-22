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
 */


@Data
@NoArgsConstructor
@Accessors(chain = true)
public class FavoriteInfo implements Parcelable {
    private String mItemId;         // 收藏点唯一码, 由AutoSDK内部生成
    private int mCommonName;        // 收藏点类型（1家，2公司，0普通收藏点）
    private String mTag;            // 附加标签
    private String mType;           // 类型
    private String mNewType;        //  新类型，预留
    private String mCustomName;    // 自定义名称 重命名时编辑的字段
    private String mClassification; //  类别
    private long mTopTime;         // 置顶操作内部更新时间
    private long mUpdateTime;

    public String getItemId() {
        return mItemId;
    }

    /**
     * 设置收藏点唯一码
     * @param itemId 收藏点唯一码
     * @return FavoriteInfo
     */
    public FavoriteInfo setItemId(final String itemId) {
        this.mItemId = itemId;
        return this;
    }

    public int getCommonName() {
        return mCommonName;
    }

    /**
     * 设置收藏点类型
     * @param commonName 收藏点类型
     * @return FavoriteInfo
     */
    public FavoriteInfo setCommonName(final int commonName) {
        this.mCommonName = commonName;
        return this;
    }

    public String getTag() {
        return mTag;
    }

    /**
     * 设置附加标签
     * @param tag 附加标签
     * @return FavoriteInfo
     */
    public FavoriteInfo setTag(final String tag) {
        this.mTag = tag;
        return this;
    }

    public String getType() {
        return mType;
    }

    /**
     * 设置类型
     * @param type 类型
     * @return FavoriteInfo
     */
    public FavoriteInfo setType(final String type) {
        this.mType = type;
        return this;
    }

    public String getNewType() {
        return mNewType;
    }

    /**
     * 设置新类型
     * @param newType 新类型
     * @return FavoriteInfo
     */
    public FavoriteInfo setNewType(final String newType) {
        this.mNewType = newType;
        return this;
    }

    public String getCustom_name() {
        return mCustomName;
    }

    /**
     * 设置自定义名称
     * @param customName 自定义名称
     * @return FavoriteInfo
     */
    public FavoriteInfo setCustom_name(final String customName) {
        this.mCustomName = customName;
        return this;
    }

    public String getClassification() {
        return mClassification;
    }

    /**
     * 设置分类
     * @param classification 分类
     * @return FavoriteInfo
     */
    public FavoriteInfo setClassification(final String classification) {
        this.mClassification = classification;
        return this;
    }

    public long getTop_time() {
        return mTopTime;
    }

    /**
     * 设置置顶时间
     * @param topTime 置顶时间
     * @return FavoriteInfo
     */
    public FavoriteInfo setTop_time(final long topTime) {
        this.mTopTime = topTime;
        return this;
    }

    public long getUpdateTime() {
        return mUpdateTime;
    }

    /**
     * 设置更新时间
     * @param updateTime 更新时间
     * @return FavoriteInfo
     */
    public FavoriteInfo setUpdateTime(final long updateTime) {
        this.mUpdateTime = updateTime;
        return this;
    }

    protected FavoriteInfo(final Parcel in) {
        mItemId = in.readString();
        mCommonName = in.readInt();
        mTag = in.readString();
        mType = in.readString();
        mNewType = in.readString();
        mCustomName = in.readString();
        mClassification = in.readString();
        mTopTime = in.readLong();
        mUpdateTime = in.readLong();
    }

    public static final Creator<FavoriteInfo> CREATOR = new Creator<FavoriteInfo>() {
        @Override
        public FavoriteInfo createFromParcel(final Parcel in) {
            return new FavoriteInfo(in);
        }

        @Override
        public FavoriteInfo[] newArray(final int size) {
            return new FavoriteInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeString(mItemId);
        parcel.writeInt(mCommonName);
        parcel.writeString(mTag);
        parcel.writeString(mType);
        parcel.writeString(mNewType);
        parcel.writeString(mCustomName);
        parcel.writeString(mClassification);
        parcel.writeLong(mTopTime);
        parcel.writeLong(mUpdateTime);
    }
}
