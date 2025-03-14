package com.fy.navi.service.adapter.layer.bls.manager;

import com.android.utils.log.Logger;
import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.model.LayerTexture;

public class CrossLayerStyle extends BaseLayerStyle {
    protected CrossLayerStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
    }

    /**
     * @return void         无返回值
     * @brief 根据放大路口类型隐藏对应的路口大图
     * @note thread：main
     */
    public void hideCross(int type) {
        if (mBizRoadCrossControl != null) {
            Logger.d("CrossImage_tag hideCross " + type);
            mBizRoadCrossControl.hideCross(type);
        }
    }

    public void setVisible(int type, boolean bVisible) {
        if (mBizRoadCrossControl != null) {
            Logger.d("CrossImage_tag hideCross " + type);
            mBizRoadCrossControl.setVisible(type, bVisible);
        }
    }

    /**
     * @return void   无返回值
     * @brief 根据放大路口类型进行显示隐藏控制
     * @note 只是显示或者隐藏控制，不涉及图片数据处理
     * @note thread：main
     */
    public void showCross(int type) {
        if (mBizRoadCrossControl != null) {
            Logger.d("CrossImage_tag showCross , type = {?} ", type);
            mBizRoadCrossControl.setVisible(type, true);
        }
    }

    public void setCrossImageInfo(CrossImageInfo crossImageInfo) {
        if (mBizRoadCrossControl != null) {
            Logger.d("CrossImage_tag setCrossImageInfo ");
            mBizRoadCrossControl.setCrossImageInfo(crossImageInfo);
        }
    }

    public boolean updateCross(byte[] buff, int crossType) {
        Logger.d("CrossImage_tag updateCross ");
        return mBizRoadCrossControl != null && mBizRoadCrossControl.updateCross(buff, crossType);
    }

    public boolean setRasterImageData(LayerTexture arrowImage, LayerTexture roadImage) {
        Logger.d("CrossImage_tag setRasterImageData ");
        return mBizRoadCrossControl != null && mBizRoadCrossControl.setRasterImageData(arrowImage, roadImage);
    }
}
