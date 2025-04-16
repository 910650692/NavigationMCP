package com.fy.navi.service.adapter.layer.bls.texture;

import com.autonavi.gbl.map.layer.model.LayerIconAnchor;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class LayerTextureMarkerInfo {
    @LayerIconAnchor.LayerIconAnchor1
    private int anchor = LayerIconAnchor.LayerIconAnchorCenter;
    private float x_ratio = 0;
    private float y_ratio = 0;

    @Override
    public String toString() {
        return "LayerTextureMarkerInfo{" +
                "anchor=" + anchor +
                ", x_ratio=" + x_ratio +
                ", y_ratio=" + y_ratio +
                '}';
    }
}
