package com.fy.navi.service.adapter.layer.bls.style;

import com.autonavi.gbl.layer.observer.PrepareLayerParamInner;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.layer.refix.LayerItemData;

public class BaseStyleAdapter extends PrepareLayerParamInner {

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    public BaseStyleAdapter(int engineID) {
        super(engineID);
    }

    /**
     * 根据item的类型提供对应的item的json文件名
     *
     * @param item
     * @return
     */
    public String provideLayerItemStyleJson(LayerItem item) {
        return null;
    }

    /**
     * 提供更新布局类型的item的数据源
     *
     * @param item
     * @return
     */
    public LayerItemData provideLayerItemData(LayerItem item) {
        return null;
    }

    /**
     * 根据item提供处理布局样式的处理器
     *
     * @param item
     * @return
     */
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        return null;
    }

    public boolean isNeedReCreate(LayerItem item) {
        return false;
    }
}
