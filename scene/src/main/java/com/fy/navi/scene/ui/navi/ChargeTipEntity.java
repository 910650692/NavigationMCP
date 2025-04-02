package com.fy.navi.scene.ui.navi;

public class ChargeTipEntity {
    private String mTitle;
    private String mSubTitle;
    private String mAction;
    @SceneNaviChargeBtnType.Type
    private int mType;
    private String mTtsContent;

    public String getTitle() {
        return mTitle;
    }

    public void setTitle(final String title) {
        this.mTitle = title;
    }

    public String getSubTitle() {
        return mSubTitle;
    }

    public void setSubTitle(final String subTitle) {
        this.mSubTitle = subTitle;
    }

    public String getAction() {
        return mAction;
    }

    public void setAction(final String action) {
        this.mAction = action;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public String getTtsContent() {
        return mTtsContent;
    }

    public void setTtsContent(final String ttsContent) {
        this.mTtsContent = ttsContent;
    }
}
