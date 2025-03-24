package com.fy.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

/**
 * BizCruiseCongestionInfo
 */
@Setter
@Getter
public class LayerItemSearchParent extends LayerItemBase {
    private LayItemSearchParentType type = LayItemSearchParentType.SEARCH_PARENT_NONE;
}
