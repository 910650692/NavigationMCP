package com.fy.navi.service.define.route;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteLightBarItem {
    private int mStatus = 0; //0:畅通, 1:缓行, 2:拥堵, 3:严重拥堵, 5:极度畅通
    private int mPercent = 0; //占路线百分比 20: 20%
}
