package com.sgm.navi.service.define.cruise;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Description: 使用 Lombok 自动生成样板代码：
 * •@Data：自动生成 Getter、Setter、toString、equals 和 hashCode。
 * •@NoArgsConstructor：生成无参构造方法。
 * •@Accessors(chain = true)：支持链式调用，例如 entity.setName("POI").setCityCode("123");。
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class CruiseParamEntity {
    private int type;//类型
    public long cameraNum = 3L;
    public int mode = 7;

    public boolean trEnable = true;
}
