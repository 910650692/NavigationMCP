package com.fy.navi.fsa.bean;

/**
 * 1.1.11、获取放大图信息
 */
public class EnlargeMap {
    /**
     * type	int	放大图类型
     * 0 – 图片类型
     * 1 – 矢量图类型
     */
    private int type;
    /**
     * status	int	放大图状态
     * -1 – 无效值
     * 0 – 展示
     * 1 – 更新
     * 2 – 隐藏
     */
    private int status;
    /**
     * ilsImageViewInfo	ILSImageViewInfo	图片放大图信息
     */
    private ILSImageViewInfo ilsImageViewInfo;
    /**
     * junctionViewInfo	JunctionViewInfo	矢量放大图信息
     */
    private JunctionViewInfo junctionViewInfo;

    public EnlargeMap() {
    }

    public EnlargeMap(int type, int status, ILSImageViewInfo ilsImageViewInfo, JunctionViewInfo junctionViewInfo) {
        this.type = type;
        this.status = status;
        this.ilsImageViewInfo = ilsImageViewInfo;
        this.junctionViewInfo = junctionViewInfo;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public ILSImageViewInfo getIlsImageViewInfo() {
        return ilsImageViewInfo;
    }

    public void setIlsImageViewInfo(ILSImageViewInfo ilsImageViewInfo) {
        this.ilsImageViewInfo = ilsImageViewInfo;
    }

    public JunctionViewInfo getJunctionViewInfo() {
        return junctionViewInfo;
    }

    public void setJunctionViewInfo(JunctionViewInfo junctionViewInfo) {
        this.junctionViewInfo = junctionViewInfo;
    }

    @Override
    public String toString() {
        return "EnlargeMap{" +
                "type=" + type +
                ", status=" + status +
                ", ilsImageViewInfo=" + ilsImageViewInfo +
                ", junctionViewInfo=" + junctionViewInfo +
                '}';
    }
}
