package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizClickLabelType;
import com.autonavi.gbl.layer.model.FlylineDrawMode;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.OperatorPosture;
import com.autonavi.gbl.map.layer.observer.ILayerClickObserver;
import com.autonavi.gbl.map.layer.observer.IPrepareLayerStyle;
import com.autonavi.gbl.map.observer.IMapEventObserver;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.FlyLineEndPointStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;

public class LayerFlyLineImpl extends BaseLayerImpl<FlyLineEndPointStyleAdapter> {
    private  FlyLineEndPointStyleAdapter flyLineEndPointStyleAdapter;
    private final IMapEventObserver pObserver;
    public LayerFlyLineImpl(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        setStyle(this);
        addClickObserver(this);
//        setVisible(true, true);
        updateDrawMode(FlylineDrawMode.FLYLINE_CLICKED_NORMAL_END, true);
        pObserver = new IMapEventObserver() {
            @Override
            public boolean onMapMoveStart() {
                return false;
            }

            @Override
            public boolean onMapMoveEnd() {
                OperatorPosture operatorPosture = getMapView().getOperatorPosture();
                if (null != operatorPosture) {
                    Coord3DDouble coord3DDouble = operatorPosture.getMapCenter();
                    GeoPoint descPoint = new GeoPoint(coord3DDouble.lon, coord3DDouble.lat);
                    for (ILayerAdapterCallBack callBack : getCallBacks()) {
                        callBack.onFlyLineMoveEnd(descPoint);
                    }
                }
                return false;
            }
        };

    }

    @Override
    protected FlyLineEndPointStyleAdapter createStyleAdapter() {
         flyLineEndPointStyleAdapter = new FlyLineEndPointStyleAdapter(getLayerFlyLineControl());
        return flyLineEndPointStyleAdapter;
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
        flyLineEndPointStyleAdapter.setVisible(bShowLine);
        if (bShowLine) {
            updateDrawMode(FlylineDrawMode.FLYLINE_CLICKED_NORMAL_END, true);
            getMapView().addMapEventObserver(pObserver);
            setClickLabelType(BizClickLabelType.ClickTypeLabel);
            updateStyle();
        } else {
            updateDrawMode(FlylineDrawMode.FLYLINE_NONE_END, true);
            getMapView().removeMapEventObserver(pObserver);
            setClickLabelType(BizClickLabelType.ClickTypeNone);
            updateStyle();
        }
    }

    /* 隐藏飞线一次，目前应用于回车位场景 */
    public void hideOnce() {
        getLayerFlyLineControl().hideOnce();
    }

    /* 设置图层样式回调接口 */
    public void setStyle(IPrepareLayerStyle pStyle) {
        getLayerFlyLineControl().setStyle(pStyle);
    }

    /* 更新样式 */
    public void updateStyle() {
        getLayerFlyLineControl().updateStyle();
    }

    /* 添加点击观察者回调 */
    public void addClickObserver(ILayerClickObserver pObserver) {
        getLayerFlyLineControl().addClickObserver(pObserver);
    }

    /* 移除点击观察者回调 */
    public void removeClickObserver(ILayerClickObserver pObserver) {
        getLayerFlyLineControl().removeClickObserver(pObserver);
    }

    /* 设置飞线是否可点击 */
    public void setClickable(boolean bClickable) {
        getLayerFlyLineControl().setClickable(bClickable);
    }

    /* 获取飞线是否可点击 */
    public boolean getClickable() {
        return getLayerFlyLineControl().getClickable();
    }

    /* 设置飞线点击到的元素类型 */
    public void setClickLabelType(@BizClickLabelType.BizClickLabelType1 int clickType) {
        getLayerFlyLineControl().setClickLabelType(clickType);
    }

    /* 获取飞线点击到的元素类型 */
    public int getClickLabelType() {
        return getLayerFlyLineControl().getClickLabelType();
    }


}
