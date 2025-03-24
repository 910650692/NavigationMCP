package com.fy.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemRouteDrawStyle extends LayerItemBase {
    public boolean bPassGrey;
    public boolean mIsOffLine;
    public boolean mIsNavi;
    public int mRouteMapMode;
    public int mRouteScene;
    public boolean mIsMultipleMode;
    public int mainPathStyleType;
}
