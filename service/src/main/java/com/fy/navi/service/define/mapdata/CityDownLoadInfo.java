package com.fy.navi.service.define.mapdata;

import java.math.BigInteger;

/**
 * @Description
 * @Author fh
 * @date 2024/12/12
 */
public class CityDownLoadInfo {
    public int adcode;
    // 是否有效下载项
    public boolean bValidItem;
    // 下载任务执行的状态
    public int taskState;
    public String statusTip;
    // 操作错误码
    public int errCode;
    // 下载进度
    public float percent;
    // 是否有数据更新
    public boolean bUpdate;
    // 是否有在用数据
    public boolean bIsDataUsed;
    // 是否发现完整高版本数据
    public boolean IsCompltelyHighVer;
    //  全量zip包解压后大小，单位byte
    public BigInteger nFullUnpackSize;
    // 增量包解压后大小，单位Byte
    public BigInteger nUnpackSize;
    // 全量zip包大小，单位byte
    public BigInteger nFullZipSize;
    // 增量zip包大小，单位Byte
    public BigInteger nZipSize;

    public int getAdCode() {
        return adcode;
    }

    public void setAdCode(int adCode) {
        this.adcode = adCode;
    }

    public boolean isbValidItem() {
        return bValidItem;
    }

    public void setbValidItem(boolean bValidItem) {
        this.bValidItem = bValidItem;
    }

    public int getTaskState() {
        return taskState;
    }

    public void setTaskState(int taskState) {
        this.taskState = taskState;
    }

    public int getErrCode() {
        return errCode;
    }

    public void setErrCode(int errCode) {
        this.errCode = errCode;
    }

    public float getPercent() {
        return percent;
    }

    public void setPercent(float percent) {
        this.percent = percent;
    }

    public boolean isbUpdate() {
        return bUpdate;
    }

    public void setbUpdate(boolean bUpdate) {
        this.bUpdate = bUpdate;
    }

    public boolean isbIsDataUsed() {
        return bIsDataUsed;
    }

    public void setbIsDataUsed(boolean bIsDataUsed) {
        this.bIsDataUsed = bIsDataUsed;
    }

    public boolean isCompletelyHighVer() {
        return IsCompltelyHighVer;
    }

    public void setCompletelyHighVer(boolean completelyHighVer) {
        IsCompltelyHighVer = completelyHighVer;
    }

    public BigInteger getnFullUnpackSize() {
        return nFullUnpackSize;
    }

    public void setnFullUnpackSize(BigInteger nFullUnpackSize) {
        this.nFullUnpackSize = nFullUnpackSize;
    }

    public BigInteger getnFullZipSize() {
        return nFullZipSize;
    }

    public void setnFullZipSize(BigInteger nFullZipSize) {
        this.nFullZipSize = nFullZipSize;
    }

    public BigInteger getnUnpackSize() {
        return nUnpackSize;
    }

    public void setnUnpackSize(BigInteger nUnpackSize) {
        this.nUnpackSize = nUnpackSize;
    }

    public BigInteger getnZipSize() {
        return nZipSize;
    }

    public void setnZipSize(BigInteger nZipSize) {
        this.nZipSize = nZipSize;
    }

}
