package com.sgm.navi.service.adapter.layer.bls.texture;

import com.autonavi.gbl.map.layer.model.LayerIconAnchor;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
/**
 * "poi_marker_info": "{\"anchor\":4,\"x_ratio\":0.0,\"y_ratio\":0.0}"
 * "poi_marker_info": "{\"anchor\":4,\"x_ratio\":0.0,\"y_ratio\":0.0,\"x_offset\":0,\"y_offset\":0}"
 */
public class TextureMarkerInfo {
    @LayerIconAnchor.LayerIconAnchor1
    private int anchor = LayerIconAnchor.LayerIconAnchorCenter;
    private float x_ratio = 0;
    private float y_ratio = 0;
    private long x_offset = 0;
    private long y_offset = 0;

    @Override
    public String toString() {
        return "TextureMarkerInfo{" +
                "anchor=" + anchor +
                ", x_ratio=" + x_ratio +
                ", y_ratio=" + y_ratio +
                ", x_offset=" + x_offset +
                ", y_offset=" + y_offset +
                '}';
    }
}
