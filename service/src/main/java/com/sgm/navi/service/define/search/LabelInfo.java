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
public class LabelInfo implements Parcelable {
    private String mContent;       // 标签名称
    private int mSubType;       // 子标签type
    private int mType;             // 标签type

    protected LabelInfo(final Parcel in) {
        mContent = in.readString();
        mType = in.readInt();
        mSubType = in.readInt();
    }

    public static final Creator<LabelInfo> CREATOR = new Creator<LabelInfo>() {
        @Override
        public LabelInfo createFromParcel(final Parcel in) {
            return new LabelInfo(in);
        }

        @Override
        public LabelInfo[] newArray(final int size) {
            return new LabelInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeString(mContent);
        parcel.writeInt(mType);
        parcel.writeInt(mSubType);
    }
}
