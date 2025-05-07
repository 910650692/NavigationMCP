1.根据业务逻辑先找到具体的对应的高德业务图层的业务接口能力
	例如 搜索图层的 充电桩 扎点接口
	/* 充电桩扎标图层业务 */
    boolean com.autonavi.gbl.layer.BizSearchControl.updateSearchChargeStation(ArrayList< BizSearchChargeStationInfo > pointList)

2.根据业务接口能力和高德的图层链路导图，确定默认图元样式是否符合UI设计，如果不符合则走自定义图元逻辑.

3.如果对应的业务图元需要走自定义图元，则先找到对应图层的XXXStyleAdapter类 例如 LayerSearchStyleAdapter，
然后先复写 provideLayerItemStyleJson 方法 返回对应图元样式的json文件名，一般情况下可以根据对应的图元的 biztype做逻辑
	图元的json配置如下
	{
      "point_layer_item_style": {
         "normal_style": {
         "poi_marker_id": "layer_layout_search_along_way_charge_normal",// 普通状态的图片资源名或者布局名
         "poi_marker_info": "{\"anchor\": 9,\"y_ratio\": 1,\"x_ratio\": 1}"//纹理的锚点坐标配置
        },
         "focus_style": {
         "poi_marker_id": "layer_layout_search_along_way_charge_focused",//点击状态的图片资源名或者不具名
         "poi_marker_info": "{\"anchor\": 9,\"y_ratio\": 1,\"x_ratio\": 1}"//纹理的锚点坐标配置
        }
      }
    }

4.如果 poi_marker_id 提供的是布局，并且布局需要根据对应数据进行赋值显示的话，可以复写 provideLayerItemData 方法 提供布局需要的数据对象
    一般情况下可以根据 对应的图元的 biztype做逻辑

5.然后在复写 方法 provideUpdateBitmapViewProcessor 提供对应更新具体的布局样式的 IUpdateBitmapViewProcessor 对象，
        并且在IUpdateBitmapViewProcessor 对应的方法onFocusProcess和 onNormalProcess进行布局更新 。