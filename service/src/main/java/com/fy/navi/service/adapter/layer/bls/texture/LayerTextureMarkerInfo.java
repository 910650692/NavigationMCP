package com.fy.navi.service.adapter.layer.bls.texture;

import com.autonavi.gbl.map.layer.model.LayerIconAnchor;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class LayerTextureMarkerInfo {
    private int repeat;
    @LayerIconAnchor.LayerIconAnchor1
    private int anchor;
    private float x_ratio;
    private float y_ratio;
    private int gen_mipmaps;
    private int pre_mul_alpha;
    private long x_offset;
    private long y_offset;

    @Override
    public String toString() {
        return "LayerTextureMarkerInfo{" +
                "repeat=" + repeat +
                ", anchor=" + anchor +
                ", x_ratio=" + x_ratio +
                ", y_ratio=" + y_ratio +
                ", gen_mipmaps=" + gen_mipmaps +
                ", pre_mul_alpha=" + pre_mul_alpha +
                ", x_offset=" + x_offset +
                ", y_offset=" + y_offset +
                '}';
    }
}
