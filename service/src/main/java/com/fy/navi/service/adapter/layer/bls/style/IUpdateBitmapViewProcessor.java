package com.fy.navi.service.adapter.layer.bls.style;

import android.view.View;

import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.define.layer.refix.LayerItemData;

public interface IUpdateBitmapViewProcessor<D extends LayerItemData> {

    default void onFocusProcess(View rootView, D data) {
    }

    default void onNormalProcess(View rootView, D data) {
    }

    default void onFocusProcess(LayerItem layerItem, View rootView, D data) {

    }

    default void onNormalProcess(LayerItem layerItem, View rootView, D data) {

    }
}
