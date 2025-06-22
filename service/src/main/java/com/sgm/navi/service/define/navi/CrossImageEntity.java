package com.sgm.navi.service.define.navi;

import com.sgm.navi.service.adapter.navi.NaviConstant;

import java.math.BigInteger;
import java.util.Arrays;

/**
 * 路口大图实体类
 * @author sgm
 * @version $Revision.*$
 */
public class CrossImageEntity {
    @NaviConstant.CrossType
    private int mType;             //路口大图类型:栅格/矢量/精品三维
    private int mVectorType;       //矢量路口大图类型:普通/近接/混淆/环岛
    private byte[] mDataBuf;       //路口大图二进制数据,栅格为jpeg,尺寸为500*320，矢量和精品3D为自定义二进制格式
    private byte[] mArrowDataBuf;  //栅格路口大图二进制数据,栅格为PNG,尺寸为500*320，其他路口大图类型为空
    private boolean mIsOnlyVector; //特殊项目使用，路口透出多类型大图功能打开且矢量图透出时生效:是否只有矢量图显示 false : 不是， true : 是
    private long mDistance;        //当前位置距路口大图距离
    private BigInteger crossImageID;

    public BigInteger getCrossImageID() {
        return crossImageID;
    }

    public void setCrossImageID(BigInteger crossImageID) {
        this.crossImageID = crossImageID;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public int getVectorType() {
        return mVectorType;
    }

    public void setVectorType(final int vectorType) {
        this.mVectorType = vectorType;
    }

    public byte[] getDataBuf() {
        return mDataBuf;
    }

    public void setDataBuf(final byte[] dataBuf) {
        this.mDataBuf = dataBuf;
    }

    public byte[] getArrowDataBuf() {
        return mArrowDataBuf;
    }

    public void setArrowDataBuf(final byte[] arrowDataBuf) {
        this.mArrowDataBuf = arrowDataBuf;
    }

    public boolean isOnlyVector() {
        return mIsOnlyVector;
    }

    public void setOnlyVector(final boolean onlyVector) {
        mIsOnlyVector = onlyVector;
    }

    public long getDistance() {
        return mDistance;
    }

    public void setDistance(final long distance) {
        this.mDistance = distance;
    }

    @Override
    public String toString() {
        return "CrossImageEntity{" +
                "mType=" + mType +
                ", mVectorType=" + mVectorType +
                ", mDataBuf=" + Arrays.toString(mDataBuf) +
                ", mArrowDataBuf=" + Arrays.toString(mArrowDataBuf) +
                ", mIsOnlyVector=" + mIsOnlyVector +
                ", mDistance=" + mDistance +
                ", crossImageID=" + crossImageID +
                '}';
    }
}
