package com.fy.navi.service.adapter.layer.bls.refix;

import android.content.Context;

import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.FlylineDrawMode;
import com.autonavi.gbl.map.MapView;

public class LayerFlyLine extends BaseLayerImpl {

    public LayerFlyLine(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerFlyLineControl().setStyle(this);
        getLayerFlyLineControl().addClickObserver(this);
    }


    /* 更新飞线终点绘制模式 */
    public void updateDrawMode(@FlylineDrawMode.FlylineDrawMode1 int drawMode, boolean bAnim) {
        getLayerFlyLineControl().updateDrawMode(drawMode, bAnim);
    }

    /* 获取飞线末端选点气泡的绘制模式 */
    public int getDrawMode() {
        return getLayerFlyLineControl().getDrawMode();
    }

    /* 设置飞线显隐 */
    public void setVisible(boolean bShowLine, boolean bShowPoint) {
        getLayerFlyLineControl().setVisible(bShowLine, bShowPoint);
    }

    /* 隐藏飞线一次，目前应用于回车位场景 */
    public void hideOnce() {
        getLayerFlyLineControl().hideOnce();
    }

}
