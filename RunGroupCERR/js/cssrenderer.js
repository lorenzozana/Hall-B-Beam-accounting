/* =========================================================================

Prototype of a CSS3 Renderer for Three.js by Keith Clark

Yep, it's a prototype. That means there's no optimisation at all! This is
just an experiment to see if CSS can be used to render Three.js geometry.

twitter.com/keithclarkcouk
www.keithclark.co.uk/labs
blog.keithclark.co.uk

----------------------------------------------------------------------------

Support:

* Geometry rendering (only for THREE.Face4 geometry)
* Textures (limited)
* Lighting


Known Issues:

* Texture UVs don't work


To do:

* Optimise CSS utils - too many nested function calls
* Refactor - don't pollute the global namespace

Hacking:

(run this in your browser console to get CssRenderer running in Three.js demos)

var rendererElement = renderer.domElement, s = document.createElement("script");s.src="/path/to/cssrenderer.js";s.onload=function(){renderer = new THREE.CssRenderer(); rendererElement.parentNode.replaceChild(renderer.domElement, rendererElement);renderer.setSize( window.innerWidth, window.innerHeight )};document.documentElement.appendChild(s);

========================================================================= */


/* CSS Utility functions - these will become private
------------------------------------------------------------------------- */

var vendorPrefixes = ["","-moz-","-webkit-","-o-","-ms-"];
var vendorLookups = {
	"perspective": "-webkit-perspective",
	"transform": "-webkit-transform",
	"linear-gradient": "-webkit-linear-gradient",
	"transform-origin": "-webkit-transform-origin",
	"transform-style": "-webkit-transform-style",
	"backface-visibility": "-webkit-backface-visibility"
}

for (var d=0;d<vendorPrefixes.length;d++) {
	if (getComputedStyle(document.body)[vendorPrefixes[d] + "transform"] !== undefined) {
		for (var venderLookup in vendorLookups) {
			vendorLookups[venderLookup] = vendorPrefixes[d] + venderLookup;
		}
		break;
	}
}

function css3d() {
	return cssProp("position", "absolute") +
		cssProp("left", 50 ,"%") +
		cssProp("top", 50 ,"%") +
		cssProp("transform-style", "preserve-3d") +
		cssProp("transform-origin", "0 0 ") +
		cssProp("backface-visibility", "hidden") +
		cssProp("background-size", "100% 100%");
}
function cssBackgroundValue(prop, value) {
	return (vendorLookups[prop] || prop) + "(" + value + ") ";
}
function cssValue(value, unit) {
	return (typeof value === "number" ? value.toFixed(4) : value) + (unit || "");
}
function cssProp(name, value, unit) {
	return (vendorLookups[name] || name) + ":" + cssValue(value, unit) + ";";
}
function cssColor(r,g,b,a) {
	return a === undefined ?
		"rgb(" + r + "," + g + "," + b + ")"
	:
		"rgba(" + r + "," + g + "," + b + "," + cssValue(a) + ")";
}

/* from Mr doob's CSS3DRenderer */
var epsilon = function ( value ) {
	return Math.abs( value ) < 0.000001 ? 0 : value;
};

var getCameraCSSMatrix = function ( matrix ) {
	var elements = matrix.elements;
	return 'matrix3d(' +
		epsilon( elements[ 0 ] ) + ',' +
		epsilon( - elements[ 1 ] ) + ',' +
		epsilon( elements[ 2 ] ) + ',' +
		epsilon( elements[ 3 ] ) + ',' +
		epsilon( elements[ 4 ] ) + ',' +
		epsilon( - elements[ 5 ] ) + ',' +
		epsilon( elements[ 6 ] ) + ',' +
		epsilon( elements[ 7 ] ) + ',' +
		epsilon( elements[ 8 ] ) + ',' +
		epsilon( - elements[ 9 ] ) + ',' +
		epsilon( elements[ 10 ] ) + ',' +
		epsilon( elements[ 11 ] ) + ',' +
		epsilon( elements[ 12 ] ) + ',' +
		epsilon( - elements[ 13 ] ) + ',' +
		epsilon( elements[ 14 ] ) + ',' +
		epsilon( elements[ 15 ] ) +
	')';
};
/* ^ from Mr doob's CSS3DRenderer  */

/* THREE.DOMRenderable
------------------------------------------------------------------------- */


THREE.DOMRenderable = function() {
	this.node = document.createElement("div");
	this.node.style.cssText = css3d();
};

THREE.DOMRenderable.prototype = {
	repaint: function() {
		var c = this.getChangedPropertyCount();
		if ( c > 0 ) {
			if ( c > 2 ) { // if more than 2 properties are changing, hide the node for faster repaints.
				this.node.style.display = "none";
			}
			
			if (this._color !== this.color) {
				this.node.style.background = this.color;
				this._color = this.color;
			}
			if (this._width !== this.width) {
				this.node.style.width = this.width + "px";
				this._width = this.width;
			}
			if (this._height !== this.height) {
				this.node.style.height = this.height + "px";
				this._height = this.height;
			}
			if (this._rotateX !== this.rotateX || this._rotateY !== this.rotateY || this._rotateZ !== this.rotateZ ||
				this._posX !== this.posX || this._posY !== this.posY || this._posZ !== this.posZ) {
				
				this.node.style[vendorLookups["transform"]] = "translate3d(" +
					cssValue(this.posX, "px") + "," +
					cssValue(this.posY, "px") + "," +
					cssValue(this.posZ, "px") + ") " +
					"rotateX(" + cssValue(this.rotateX, "rad") + ") " +
					"rotateY(" + cssValue(this.rotateY, "rad") + ") " +
					"rotateZ(" + cssValue(this.rotateZ, "rad") + ")";

				this._rotateX = this.rotateX;
				this._rotateY = this.rotateY;
				this._rotateZ = this.rotateZ;
				this._posX = this.posX;
				this._posY = this.posY;
				this._posZ = this.posZ;
			}

			if ( c > 2 ) {
				this.node.style.display = "";
			}
		}
		return c !== 0;
	},
	getChangedPropertyCount: function() {
		return (this._color != this.color) +
			(this._width != this.width) +
			(this._height != this.height) +
			(this._rotateX != this.rotateX || this._rotateY != this.rotateY || this._rotateZ != this.rotateZ || this._posX != this.posX || this._posY != this.posY || this._posZ != this.posZ);
	}/*,
	repaint_: function() {
		var cssText = "";
		if (this._color != this.color) {
			cssText += cssProp("background", this.color);
			this._color = this.color;
		}
		if (this._width != this.width) {
			cssText += cssProp("width", this.width + "px");
			this._width = this.width;
		}
		if (this._height != this.height) {
			cssText += cssProp("height", this.height + "px");
			this._height = this.height;
		}
		if (this._rotateX != this.rotateX || this._rotateY != this.rotateY || this._rotateZ != this.rotateZ ||
			this._posX != this.posX || this._posY != this.posY || this._posZ != this.posZ) {
			var transform =
				"translate3d(" + cssValue(this.posX, "px") + "," +
					cssValue(this.posY, "px") + "," +
					cssValue(this.posZ, "px") + ") " +
				"rotateX(" + cssValue(-this.rotateX, "rad") + ") " +
				"rotateY(" + cssValue(this.rotateY, "rad") + ") " +
				"rotateZ(" + cssValue(-this.rotateZ, "rad") + ")";
				cssText += cssProp("transform", transform);
			this._rotateX = this.rotateX;
			this._rotateY = this.rotateY;
			this._rotateZ = this.rotateZ;
			this._posX = this.posX;
			this._posY = this.posY;
			this._posZ = this.posZ;
		}
		if (cssText) {
			this.node.style.cssText += cssText;
		}
	}*/
}

/* CSS Renderer
------------------------------------------------------------------------- */

THREE.CssRenderer = function ( parameters ) {

	var _renderData, _objects, _lights,

	_v1, _v2, _v3, _v4,

	_repaints, // debugging

	_height,
	_projector = new THREE.Projector(),
	_vector3  = new THREE.Vector3(),
	_normalMatrix = new THREE.Matrix3(),

	_color = new THREE.Color(),
	
	_enableLighting = false,
	_ambientLight = new THREE.Color(),
	_directionalLights = new THREE.Color(),
	_pointLights = new THREE.Color(),
	_diffuseColor = new THREE.Color(),
	_emissiveColor = new THREE.Color();

	this.domElement = document.createElement("div");
	this.cameraElement = document.createElement("div");

	this.cameraElement.style.cssText = css3d();

	this.domElement.appendChild(this.cameraElement);

	this.getMaxAnisotropy = function() {
		return 0;
	};
	this.clear = function () {};

	this.setSize = function(width, height) {
		_height = height;
		this.domElement.style.width = width + "px";
		this.domElement.style.height = height + "px";
	};

	this.render = function(scene, camera) {
		_repaints = 0;

		var meshElement,
			faceElement,
			geometry,
			faces,
			vertices,
			faceName,
			fov = 0.5 / Math.tan( 0.0174532925 * ( camera.fov * 0.5 ) ) * _height;

		this.domElement.style.cssText += cssProp("perspective", fov, "px");

		this.cameraElement.style.cssText += cssProp("transform", "translate3d(0,0," + fov + "px)" + getCameraCSSMatrix( camera.matrixWorldInverse ));

		_renderData = _projector.projectScene( scene, camera, true );
		_objects = _renderData.objects;
		_lights = _renderData.lights;
		_enableLighting = _lights.length > 0;

		if (_enableLighting) {
			calculateLights();
		}

		for (var cl = _objects.length, c = 0; c < cl; c++) {
			var obj = _objects[c].object;

			var material = obj.material;
			if ( material === undefined || obj.visible === false ) continue;


			if (obj instanceof THREE.Mesh) {
				meshElement = obj._domElement;
				geometry = obj.geometry;
				faces = geometry.faces;
				vertices = geometry.vertices;

				if (!meshElement) {
					meshElement = obj._domElement = new THREE.DOMRenderable();
				}

				meshElement.posX = obj.position.x;
				meshElement.posY = obj.position.y;
				meshElement.posZ = obj.position.z;
				meshElement.rotateX = obj.rotation.x;
				meshElement.rotateY = obj.rotation.y;
				meshElement.rotateZ = obj.rotation.z;

				for (var il = faces.length, i = 0; i < il; i++) {
					var face = faces[i];
					if (face instanceof THREE.Face4) {
						faceName = "face" + i + "_element"
						var faceElement = obj[faceName];
						if (!faceElement) {
							faceElement = obj[faceName] = new THREE.DOMRenderable();
							meshElement.node.appendChild(faceElement.node);
						}
						renderFace4(vertices[face.a], vertices[face.b], vertices[face.c], vertices[face.d], obj, face, scene, faceElement);
						if ( faceElement.repaint() ) {
							_repaints++;
						}
					}
				}

				if (meshElement.repaint()) {
					_repaints++;
				}

				if (!meshElement.node.parentNode) {
					this.cameraElement.appendChild(meshElement.node);
				}
			}
		}
		//document.title = repaints;
	}


	function calculateLights() {
		_ambientLight.setRGB( 0, 0, 0 );
		_directionalLights.setRGB( 0, 0, 0 );
		_pointLights.setRGB( 0, 0, 0 );
		for ( var l = 0, ll = _lights.length; l < ll; l ++ ) {
			var light = _lights[ l ];
			var lightColor = light.color;
			if ( light instanceof THREE.AmbientLight ) {
				_ambientLight.r += lightColor.r;
				_ambientLight.g += lightColor.g;
				_ambientLight.b += lightColor.b;
			} else if ( light instanceof THREE.DirectionalLight ) {
				// for particles
				_directionalLights.r += lightColor.r;
				_directionalLights.g += lightColor.g;
				_directionalLights.b += lightColor.b;
			} else if ( light instanceof THREE.PointLight ) {
				// for particles
				_pointLights.r += lightColor.r;
				_pointLights.g += lightColor.g;
				_pointLights.b += lightColor.b;
			}
		}
	}

	function calculateLight( position, normal, color ) {
		var a = 0, amount;
		var light, lightColor, lightPosition;

		for ( var l = 0, ll = _lights.length; l < ll; l ++ ) {

			light = _lights[ l ];
			lightColor = light.color;

			if ( light instanceof THREE.DirectionalLight ) {

				lightPosition = light.matrixWorld.getPosition().normalize();
				amount = normal.dot( lightPosition );

				if ( amount <= 0 ) continue;

				amount *= light.intensity;

				color.r += lightColor.r * amount;
				color.g += lightColor.g * amount;
				color.b += lightColor.b * amount;

			} else if ( light instanceof THREE.PointLight ) {

				lightPosition = light.matrixWorld.getPosition();
				amount = normal.dot( _vector3.sub( lightPosition, position ).normalize() );
				if ( amount <= 0 ) continue;

				amount *= light.distance == 0 ? 1 : 1 - Math.min( position.distanceTo( lightPosition ) / light.distance, 1 );

				if ( amount == 0 ) continue;

				amount *= light.intensity;

				color.r += lightColor.r * amount;
				color.g += lightColor.g * amount;
				color.b += lightColor.b * amount;

			}
			a = Math.max( a, amount );
		}
		return a;
	}




	function renderFace4(v1, v2, v3, v4, mesh, face, scene, node) {
		var a;
		var fc = face.centroid.clone();
		var fn = face.normal.clone();
		var dx = new THREE.Vector3().sub( v2, v1 );
		var dy = new THREE.Vector3().sub( v4, v1 );
		var n = new THREE.Vector3().cross( dx, dy );
		var modelMatrix = mesh.matrixWorld;
		var material = mesh.material;

		_normalMatrix.getInverse( modelMatrix ).transpose();
		_normalMatrix.multiplyVector3( fn ).normalize();
		modelMatrix.multiplyVector3( fc );

		node.width = dx.length();
		node.height = dy.length();
		node.posX = v3.x;
		node.posY = v3.y;
		node.posZ = v3.z;

		node.rotateX = -Math.atan2( n.y, n.z );
		node.rotateY = Math.asin( n.x / n.length() );
		node.rotateZ = Math.PI/2;

		if ( Math.cos( node.rotateX ) < 0 ) {
			node.rotateZ += Math.PI;
		}

		if ( material instanceof THREE.MeshLambertMaterial || material instanceof THREE.MeshPhongMaterial ) {
			_diffuseColor.copy( material.color );
			_emissiveColor.copy( material.emissive );

			if ( material.vertexColors === THREE.FaceColors ) {
				_diffuseColor.r *= face.color.r;
				_diffuseColor.g *= face.color.g;
				_diffuseColor.b *= face.color.b;
			}

			if ( _enableLighting ) {
				_color.r = _ambientLight.r;
				_color.g = _ambientLight.g;
				_color.b = _ambientLight.b;
				a = 1 - calculateLight(fc, fn, _color);
				_color.r *= _diffuseColor.r + _emissiveColor.r;
				_color.g *= _diffuseColor.g + _emissiveColor.g;
				_color.b *= _diffuseColor.b + _emissiveColor.b;

				if ( material.map ) {
					_color.r = _color.r * 255 | 0;
					_color.g = _color.g * 255 | 0;
					_color.b = _color.b * 255 | 0;
					node.color = cssBackgroundValue("linear-gradient", cssColor(_color.r, _color.g, _color.b, a) + "," + cssColor(_color.r, _color.g, _color.b, a)) + ","+
					cssBackgroundValue("url", material.map.sourceFile) + " 0 0 / 100% 100%";
				} else {
					node.color = _color.getStyle();
				}

			} else {
				if ( material.map ) {
					node.color = cssBackgroundValue("url", material.map.sourceFile) + " 0 0 / 100% 100%";
				} else {
					_color.r = _diffuseColor.r + _emissiveColor.r;
					_color.g = _diffuseColor.g + _emissiveColor.g;
					_color.b = _diffuseColor.b + _emissiveColor.b;
					node.color = _color.getStyle();
				}
			}

		} else if ( material instanceof THREE.MeshBasicMaterial ) {
			if ( material.map ) {
				node.color = cssBackgroundValue("url", material.map.sourceFile) + " 0 0 / 100% 100%";
			} else {
				_color.copy( material.color );
				if ( material.vertexColors === THREE.FaceColors ) {
					_color.r *= face.color.r;
					_color.g *= face.color.g;
					_color.b *= face.color.b;
				}
				node.color = _color.getStyle();
			}
		} else if ( material instanceof THREE.MeshNormalMaterial ) {
			_color.r = normalToComponent( fn.x );
			_color.g = normalToComponent( fn.y );
			_color.b = normalToComponent( fn.z );
			node.color = _color.getStyle();
		}
	}

	function normalToComponent( normal ) {
		var component = ( normal + 1 ) * 0.5;
		return component < 0 ? 0 : ( component > 1 ? 1 : component );
	}

};
