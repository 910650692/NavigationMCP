package com.fy.navi.service.define.voice;

import android.annotation.SuppressLint;
import android.graphics.drawable.Drawable;

import com.android.utils.file.FileUtils;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.R;

import java.math.BigInteger;
import java.util.Locale;

public class VoiceInfo {

    private int engineType;
    private String auditionUrl;
    private String imageUrl;
    private String fileMd5;
    private boolean isNew;
    private boolean isRecommended;
    private boolean isUpdate;
    private int id;
    private int srcCode;
    private int hidden;
    private BigInteger zipDataSize;
    private BigInteger unpackDataSize;
    private int taskState;
    private float percent;
    private String name;
    private String subName;
    private String version;
    private String desc;
    private String imageFilePath;
    private String filePath;
    private boolean isEnabled;
    private boolean isUsed;

    public VoiceInfo() {
        this.engineType = 0;
        this.auditionUrl = "";
        this.imageUrl = "";
        this.fileMd5 = "";
        this.isNew = false;
        this.isRecommended = false;
        this.isUpdate = false;
        this.id = 0;
        this.srcCode = 0;
        this.hidden = 0;
        this.zipDataSize = new BigInteger("0");
        this.unpackDataSize = new BigInteger("0");
        this.taskState = 0;
        this.percent = 0.0F;
        this.name = "";
        this.subName = "";
        this.version = "";
        this.desc = "";
        this.imageFilePath = "";
        this.filePath = "";
        this.isEnabled = true;
        this.isUsed = false;
    }

    public boolean isUsed() {
        return isUsed;
    }

    public void setUsed(boolean used) {
        isUsed = used;
    }

    public boolean isEnabled() {
        this.isEnabled = true;
        switch (taskState) {
            case OperationStatus.TASK_STATUS_CODE_DOING:
            case OperationStatus.TASK_STATUS_CODE_DONE:
                this.isEnabled = false;
                break;
        }
        return isEnabled;
    }

    public void setEnabled(boolean enabled) {
        isEnabled = enabled;
    }

    public int getEngineType() {
        return engineType;
    }

    public void setEngineType(int engineType) {
        this.engineType = engineType;
    }

    public String getAuditionUrl() {
        return auditionUrl;
    }

    public void setAuditionUrl(String auditionUrl) {
        this.auditionUrl = auditionUrl;
    }

    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public String getFileMd5() {
        return fileMd5;
    }

    public void setFileMd5(String fileMd5) {
        this.fileMd5 = fileMd5;
    }

    public boolean isNew() {
        return isNew;
    }

    public void setNew(boolean aNew) {
        isNew = aNew;
    }

    public boolean isRecommended() {
        return isRecommended;
    }

    public void setRecommended(boolean recommended) {
        isRecommended = recommended;
    }

    public boolean isUpdate() {
        return isUpdate;
    }

    public void setUpdate(boolean update) {
        isUpdate = update;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getSrcCode() {
        return srcCode;
    }

    public void setSrcCode(int srcCode) {
        this.srcCode = srcCode;
    }

    public int getHidden() {
        return hidden;
    }

    public void setHidden(int hidden) {
        this.hidden = hidden;
    }

    public float getZipDataSize() {
        return zipDataSize.floatValue() / 1048576L;
    }

    public void setZipDataSize(BigInteger zipDataSize) {
        this.zipDataSize = zipDataSize;
    }

    public String getUnpackDataSize() {
        return String.format(Locale.CHINA, "%.2f", unpackDataSize.floatValue() / 1048576L) + "MB";
    }

    public float getUnpackDataSizeFloat() {
        return unpackDataSize.floatValue() / 1048576L;
    }

    public void setUnpackDataSize(BigInteger unpackDataSize) {
        this.unpackDataSize = unpackDataSize;
    }

    public int getTaskState() {
        return taskState;
    }

    public void setTaskState(int taskState) {
        this.taskState = taskState;
    }

    public int getPercent() {
        return Math.round(percent);
    }

    public void setPercent(float percent) {
        this.percent = percent;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSubName() {
        return subName;
    }

    public void setSubName(String subName) {
        this.subName = subName;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getDesc() {
        return desc;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public String getImageFilePath() {
        return imageFilePath;
    }

    public void setImageFilePath(String imageFilePath) {
        this.imageFilePath = imageFilePath;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    public Drawable getLeftDrawable() {
        Drawable drawable = null;
        boolean isDataExist = FileUtils.getInstance().checkFile(filePath);
        switch (taskState) {
            case OperationStatus.TASK_STATUS_CODE_READY:
                if (isDataExist){
                    drawable = AppCache.getInstance().getMApplication().getDrawable(R.drawable.ic_update);
                }else{
                    drawable = AppCache.getInstance().getMApplication().getDrawable(R.drawable.ic_down);
                }
                break;
            case OperationStatus.TASK_STATUS_CODE_PAUSE:
            case OperationStatus.TASK_STATUS_CODE_SUCCESS:
                drawable = AppCache.getInstance().getMApplication().getDrawable(R.drawable.ic_continue);
                break;
            case OperationStatus.TASK_STATUS_CODE_DOING:
            case OperationStatus.TASK_STATUS_CODE_DONE:
                drawable = AppCache.getInstance().getMApplication().getDrawable(R.drawable.img_pause);
                break;
            default:
                break;
        }
        return drawable;
    }

    public String getOperateText() {
        String desc = "";
        boolean isDataExist = FileUtils.getInstance().checkFile(filePath);
        switch (taskState) {
            case OperationStatus.TASK_STATUS_CODE_READY:
                if(isDataExist)
                {
                    desc = "更新";
                }
                else
                {
                    desc = "下载";
                }

                break;
            case OperationStatus.TASK_STATUS_CODE_WAITING:
                desc = "等待中";
                break;
            case OperationStatus.TASK_STATUS_CODE_PAUSE:
                desc = "继续";
                break;
            case OperationStatus.TASK_STATUS_CODE_DOING:
            case OperationStatus.TASK_STATUS_CODE_DONE:
                desc = String.format(Locale.CHINA,"%.0f", percent) + "%";
                break;
            case OperationStatus.TASK_STATUS_CODE_CHECKING:
                desc = "校验中";
                break;
            case OperationStatus.TASK_STATUS_CODE_CHECKED:
                desc = "校验完成";
                break;
            case OperationStatus.TASK_STATUS_CODE_UNZIPPING:
                desc = "解压中: " + String.format(Locale.CHINA,"%.2f", percent) + "%";
                break;
            case OperationStatus.TASK_STATUS_CODE_UNZIPPED:
                desc = "解压完成";
                break;
            case OperationStatus.TASK_STATUS_CODE_SUCCESS:
                desc = "使用";
                break;
            case OperationStatus.TASK_STATUS_CODE_ERR:
            case OperationStatus.TASK_STATUS_CODE_MAX:
                desc = "重试";
                break;
            default:
                break;
        }
        return desc;
    }
}
